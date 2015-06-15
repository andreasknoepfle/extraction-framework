package org.dbpedia.extraction.owlmappings

import java.util.logging.{Level, LogRecord, Logger}

import org.dbpedia.extraction.dataparser.StringParser
import org.dbpedia.extraction.mappings.{CalculateMapping, CombineDateMapping, ConditionMapping, ConditionalMapping, ConstantMapping, DateIntervalMapping, Extractor, GeoCoordinatesMapping, IntermediateNodeMapping, Mappings, PropertyMapping, Redirects, SimplePropertyMapping, TableMapping, TemplateMapping}
import org.dbpedia.extraction.ontology.datatypes.Datatype
import org.dbpedia.extraction.ontology.{Ontology, OntologyClass, OntologyProperty}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.TemplateNode
import org.semanticweb.owlapi.model._

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.language.reflectiveCalls

/**
 * Loads the mappings from the configuration and builds a MappingExtractor instance.
 * This should be replaced by a general loader later on, which loads the mapping objects based on the grammar (which can be defined using annotations)
 */
object MappingsLoaderOWL {
  // Note: we pass the encoded page title as log parameter so we can render a link on the server
  private val logger = Logger.getLogger(MappingsLoaderOWL.getClass.getName)

  def load(context: {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter}): Mappings = {
    logger.info("Loading mappings (" + context.language.wikiCode + ")")
    val classMappings = new HashMap[String, Extractor[TemplateNode]]()
    val tableMappings = new ArrayBuffer[TableMapping]()
    val owlClassMappings = context.mappingOntology.getClassMappings()
    logger.info(owlClassMappings.size + " Mappings found in " + context.language.wikiCode)

    for (mapping <- owlClassMappings) {
      try {
        classMappings(context.mappingOntology.getMappingLabel(mapping)) =
          context.mappingOntology.getClassMappingType(mapping) match {
          case "TemplateMapping" =>
            loadTemplateMapping(mapping, context)
          case "ConditionalMapping" =>
            loadConditionalMapping(mapping, context)
        }
      } catch {
        case e: Exception =>
          logger.warning("Could not load Mapping " + mapping.getIRI + ": " + e.getMessage)
      }
    }

    logger.info("Mappings loaded (" + context.language.wikiCode + "): " + classMappings.size)

    new Mappings(classMappings.toMap, tableMappings.toList)
  }

  private def loadTemplateMapping(mapping: OWLClass, context: {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter}) = {
    val mappingParameters = context.mappingOntology.getTemplateMappingParameters(mapping)
    new TemplateMapping(
      loadOntologyClass(mappingParameters.mapToClass, context),
      loadOntologyClass(mappingParameters.correspondingClass.orNull, context),
      loadOntologyProperty(mappingParameters.correspondingProperty.orNull, context),
      loadPropertyMappings(mapping, context),
      context)
  }

  private def loadPropertyMappings(mapping: OWLClass, context: {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter}): List[PropertyMapping] = {
    var mappings = List[PropertyMapping]()

    for (intermediateNodeMapping <- context.mappingOntology.getIntermediateNodeMappings(mapping)) {
      mappings ::= loadIntermediateNodeMapping(intermediateNodeMapping, context)
    }

    for (propertyMapping <- context.mappingOntology.getPropertyMappings(mapping)) {
      mappings ::= loadPropertyMapping(mapping, propertyMapping, context)
    }

    mappings.reverse
  }

  private def loadIntermediateNodeMapping(mapping: OWLClass, context: {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter}): IntermediateNodeMapping = {
    val mappingParameters = context.mappingOntology.getIntermediateNodeMappingParameters(mapping)
    new IntermediateNodeMapping(
      loadOntologyClass(mappingParameters.nodeClass, context),
      loadOntologyProperty(mappingParameters.correspondingProperty, context),
      loadPropertyMappings(mapping, context),
      context
    )
  }


  private def loadPropertyMapping(mapping: OWLClass, property: OWLNamedIndividual, context: {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter}) =

    context.mappingOntology.getPropertyMappingParameters(mapping, property) match {
      case SimplePropertyMappingParameters(templateProperty, ontologyProperty, select, prefix, suffix,
      transform, unit, language, factor) => {
        new SimplePropertyMapping(templateProperty,
          loadOntologyProperty(ontologyProperty, context),
          select.orNull,
          prefix.orNull,
          suffix.orNull,
          transform.orNull,
          loadDatatype(unit.orNull, context),
          loadLanguage(language.orNull),
          factor.getOrElse(1),
          context)
      }
      case DateIntervalMappingParameters(templateProperty, startDateOntologyProperty, endDateOntologyProperty) => {
        new DateIntervalMapping(templateProperty,
          loadOntologyProperty(startDateOntologyProperty, context),
          loadOntologyProperty(endDateOntologyProperty, context),
          context)
      }
      case CombineDateMappingParameters(templateProperty1, templateProperty2, templateProperty3,
      unit1, unit2, unit3, ontologyProperty) => {
        // TODO: change the syntax on the mappings wiki to allow an arbitrary number of template properties.
        val templateProperties = new HashMap[String, Datatype]()
        templateProperties(templateProperty1) = loadDatatype(unit1, context)
        templateProperties(templateProperty2) = loadDatatype(unit2, context)
        if (templateProperty3.nonEmpty)
          templateProperties(templateProperty3.get) = loadDatatype(unit3.orNull, context)
        new CombineDateMapping(
          loadOntologyProperty(ontologyProperty, context),
          templateProperties,
          context)
      }
      case CalculateMappingParameters(templateProperty1, templateProperty2,
      unit1, unit2, operation, ontologyProperty) => {
        new CalculateMapping(
          templateProperty1,
          templateProperty2,
          loadDatatype(unit1.orNull, context),
          loadDatatype(unit2.orNull, context),
          operation,
          loadOntologyProperty(ontologyProperty, context),
          context)
      }
      case GeocoordinatesMappingParameters(ontologyProperty, coordinates, latitude, longitude, longitudeDegrees, longitudeMinutes,
      longitudeSeconds, longitudeDirection, latitudeDegrees, latitudeMinutes,
      latitudeSeconds, latitudeDirection) => {
        new GeoCoordinatesMapping(
          loadOntologyProperty(ontologyProperty.orNull, context),
          coordinates.orNull,
          latitude.orNull,
          longitude.orNull,
          longitudeDegrees.orNull,
          longitudeMinutes.orNull,
          longitudeSeconds.orNull,
          longitudeDirection.orNull,
          latitudeDegrees.orNull,
          latitudeMinutes.orNull,
          latitudeSeconds.orNull,
          latitudeDirection.orNull,
          context)
      }
      case ConstantMappingParameters(ontologyProperty, value, unit) => {
        new ConstantMapping(loadOntologyProperty(ontologyProperty, context),
          value,
          loadDatatype(unit.orNull, context),
          context)
      }
    }


  private def loadConditionalMapping(mapping : OWLClass, context : {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter} ): ConditionalMapping = {

    val conditionMappings = for(condition <- context.mappingOntology.getConditionMappings(mapping))
      yield loadConditionMapping(condition, context)

    new ConditionalMapping(conditionMappings.toList,loadPropertyMappings(mapping, context))
  }

  private def loadConditionMapping(mapping: OWLClass , context : {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter} ): ConditionMapping = {
    val mappingParameters = context.mappingOntology.getConditionMappingParameters(mapping)
    new ConditionMapping(
      mappingParameters.templateProperty.orNull,
      mappingParameters.operator,
      mappingParameters.value.orNull,
      loadTemplateMapping(mapping,context)
    )
  }

  private def loadOntologyClass(iri : IRI, context : {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter} ) : OntologyClass = {
    iri match {
      case some: IRI => context.ontology.classes(context.prefixConverter.getPrefixIRI(iri))
      case null => null
    }
  }

  private def loadDatatype(iri : IRI, context : {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter} ) : Datatype = {
    iri match {
      case some: IRI => context.ontology.datatypes(context.prefixConverter.getPrefixIRI(iri))
      case null => null
    }
  }

  private def loadOntologyProperty(iri : IRI, context : {
    def ontology: Ontology
    def language: Language
    def redirects: Redirects
    def mappingOntology: MappingOntology
    def prefixConverter: OWLPrefixConverter} ) : OntologyProperty = {
    iri match {
      case some: IRI => context.ontology.properties(context.prefixConverter.getPrefixIRI(iri))
      case null => null
    }
  }

  private def loadLanguage(language: String) : Language = {
    language match {
      case lang: String => Language(lang)
      case null => null
    }
  }




}
