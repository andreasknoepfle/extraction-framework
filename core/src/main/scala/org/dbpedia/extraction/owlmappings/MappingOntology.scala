package org.dbpedia.extraction.owlmappings

import java.io.File

import com.google.common.base.Optional
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLXMLOntologyFormat
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.PrefixOWLOntologyFormat

import scala.collection.mutable.HashMap
import scala.collection.JavaConversions.asScalaSet

abstract class MappingParameters
case class TemplateMappingParameters(mapToClass: IRI,
                                     correspondingClass: Option[IRI],
                                     correspondingProperty: Option[IRI] ) extends MappingParameters
case class ConditionMappingParameters(templateProperty: Option[String],
                                      operator: String,
                                      value: Option[String]) extends MappingParameters
case class IntermediateNodeMappingParameters(nodeClass: IRI,
                                             correspondingProperty: IRI) extends MappingParameters

abstract class PropertyMappingParameters extends MappingParameters
case class SimplePropertyMappingParameters(templateProperty: String,
                                           ontologyProperty: IRI,
                                           select: Option[String],
                                           prefix: Option[String],
                                           suffix: Option[String],
                                           transform: Option[String],
                                           unit: Option[IRI],
                                           language: Option[String],
                                           factor: Option[Double]) extends PropertyMappingParameters
case class DateIntervalMappingParameters(templateProperty: String,
                                         startDateOntologyProperty: IRI,
                                         endDateOntologyProperty: IRI) extends PropertyMappingParameters
case class CombineDateMappingParameters(templateProperty1: String,
                                        templateProperty2: String,
                                        templateProperty3: Option[String],
                                        unit1: IRI,
                                        unit2: IRI,
                                        unit3: Option[IRI],
                                        ontologyProperty: IRI) extends PropertyMappingParameters
case class CalculateMappingParameters(templateProperty1: String,
                                      templateProperty2: String,
                                      unit1: Option[IRI],
                                      unit2: Option[IRI],
                                      operation: String,
                                      ontologyProperty: IRI) extends PropertyMappingParameters
case class GeocoordinatesMappingParameters(ontologyProperty: Option[IRI],
                                            coordinates: Option[String],
                                           latitude: Option[String],
                                           longitude: Option[String],
                                           longitudeDegrees: Option[String],
                                           longitudeMinutes: Option[String],
                                           longitudeSeconds: Option[String],
                                           longitudeDirection: Option[String],
                                           latitudeDegrees: Option[String],
                                           latitudeMinutes: Option[String],
                                           latitudeSeconds: Option[String],
                                           latitudeDirection: Option[String]) extends PropertyMappingParameters
case class ConstantMappingParameters(ontologyProperty: IRI,
                                     value: String,
                                     unit: Option[IRI]) extends PropertyMappingParameters

object MappingSchemaOntology {
  val schema_ontology_iri = IRI.create("http://dbpedia.org/mappings")
  val manager = OWLManager.createOWLOntologyManager
  val ontology = manager.createOntology(schema_ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  val ontologyClassCache = new HashMap[String, OWLClass]
  val annotationPropertyCache = new HashMap[String, OWLAnnotationProperty]

  def ontologyClass(className: String): OWLClass = {
    ontologyClassCache.getOrElseUpdate(className,factory.getOWLClass(ontologyClassIRI(className)))
  }

  def ontologyClassIRI(className: String): IRI = {
    IRI.create(schema_ontology_iri + "#" + className)
  }

  def annotationProperty(parameterName: String): OWLAnnotationProperty = {
    annotationPropertyCache.getOrElseUpdate(parameterName, factory.getOWLAnnotationProperty(IRI.create(schema_ontology_iri + parameterName)))
  }
}

class MappingOntology(name: String, file: File)  {

  val manager = OWLManager.createOWLOntologyManager
  val ontology_iri = IRI.create("http://dbpedia.org/mappings/" + name.replace(" ","_"))
  val ontology = if (file.exists) { manager.loadOntologyFromOntologyDocument(file) } else { manager.createOntology(ontology_iri) }
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  def saveOntology(format: PrefixOWLOntologyFormat): Unit = {
    val owlxmlformat = new OWLXMLOntologyFormat()
    owlxmlformat.copyPrefixesFrom(format)
    owlxmlformat.setPrefix("dbpedia","http://dbpedia.org/ontology/")
    owlxmlformat.setDefaultPrefix("http://dbpedia.org/mappings/")
    manager.saveOntology(ontology, owlxmlformat ,
      IRI.create(file))
  }

  def getMappingLabel(mapping: OWLClass): String = {
    mapping.getAnnotations(ontology,factory.getRDFSLabel).headOption match {
      case Some(label) => label.getValue.asInstanceOf[OWLLiteral].getLiteral
      case None => mapping.getIRI.getShortForm
    }
  }

  def getClassMappings(): Set[OWLClass] = {
    getSubclassesForClass(MappingSchemaOntology.ontologyClass("DBpediaMapping"))
  }

  def getClassMappingType(cls: OWLClass): String = {
    cls.getSuperClasses(ontology)
      .map(c => c.asInstanceOf[OWLClass].getIRI)
      .filter(_ != MappingSchemaOntology.ontologyClassIRI("DBpediaMapping"))
      .head.getRemainder.get()
  }



  def getTemplateMappingParameters(mapping: OWLClass): TemplateMappingParameters = {
    TemplateMappingParameters(
      getIRIParameter(mapping, "/templateParameter#mapToClass"),
      getOptionalIRIParameter(mapping, "/templateParameter#correspondingClass"),
      getOptionalIRIParameter(mapping, "/templateParameter#correspondingProperty")
    )
  }

  def getIntermediateNodeMappingParameters(mapping: OWLClass): IntermediateNodeMappingParameters = {
    IntermediateNodeMappingParameters(
      getIRIParameter(mapping, "/intermediateNodeParameter#nodeClass"),
      getIRIParameter(mapping, "/intermediateNodeParameter#correspondingProperty")
    )
  }

  def getConditionMappingParameters(mapping: OWLClass): ConditionMappingParameters = {
    ConditionMappingParameters(
      getOptionalStringParameter(mapping, "/conditionParameter#templatePropety"),
      getStringParameter(mapping, "/conditionParameter#operator"),
      getOptionalStringParameter(mapping, "/conditionParameter#value")
    )
  }

  def getPropertyMappings(mapping: OWLClass): Set[OWLNamedIndividual] = {
    mapping.getIndividuals(ontology).map(_.asOWLNamedIndividual()).toSet
  }

  def getIntermediateNodeMappings(mapping: OWLClass): Set[OWLClass] = {
    getSubclassesForClass(mapping).filter(
      hasSuperclassesOfType(_, MappingSchemaOntology.ontologyClassIRI("IntermediateNodeMapping"))
    )
  }

  def getConditionMappings(mapping: OWLClass): Set[OWLClass] = {
    getSubclassesForClass(mapping)
  }

  private def getSubclassesForClass(cls: OWLClass): Set[OWLClass] = {
    cls.getSubClasses(ontology)
    .map(c => c.asInstanceOf[OWLClass]).toSet
  }

  private def hasSuperclassesOfType(mapping: OWLClass, typeToCheck: IRI ): Boolean = {
    !mapping.getSuperClasses(ontology).filter(_.asOWLClass().getIRI() == typeToCheck).isEmpty
  }

  def createTemplateMapping(name: String,
                            params: TemplateMappingParameters) : OWLClass = {

    val superClasses = List (MappingSchemaOntology.ontologyClass("DBpediaMapping"),
      MappingSchemaOntology.ontologyClass("TemplateMapping"))

    val mapping = createOntologyClass(name, superClasses)
    addParameters(mapping, params)
    mapping
  }

  def createConditionalMapping(name: String): OWLClass = {
    createOntologyClass(
      name,
      List(MappingSchemaOntology.ontologyClass("DBpediaMapping"),
        MappingSchemaOntology.ontologyClass("ConditionalMapping"))
    )
  }

  def createConditionMapping(name: String,
                             conditionalMapping: OWLClass,
                             params: ConditionMappingParameters,
                             templateParams: TemplateMappingParameters): OWLClass = {
    val mapping = createOntologyClass(
      name,
      List(conditionalMapping,
        MappingSchemaOntology.ontologyClass("TemplateMapping"))
    )
    addParameters(mapping, params)
    addParameters(mapping, templateParams)
    mapping
  }

  def createIntermediateNodeMapping(parentMapping: OWLClass, params: IntermediateNodeMappingParameters): OWLClass = {
    val mapping = createOntologyClass(
      IRI.create(parentMapping.getIRI + "_" + params.nodeClass.getShortForm),
      List(parentMapping,
        MappingSchemaOntology.ontologyClass("PropertyMapping")), params.nodeClass.getShortForm)
    addParameters(mapping,params)
    mapping
  }

  def createSimplePropertyMapping(propertyName: String, parentMapping: OWLClass,
                                  params: SimplePropertyMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("PropertyMapping", propertyName, parentMapping, params)
  }

  def createDateIntervalMapping(propertyName: String, parentMapping: OWLClass,
                                  params: DateIntervalMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("DateIntervalMapping", propertyName, parentMapping, params)
  }

  def createCombineDateMapping(propertyName: String, parentMapping: OWLClass,
                                params: CombineDateMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("CombineDateMapping", propertyName, parentMapping, params)
  }

  def createCalculateMapping(propertyName: String, parentMapping: OWLClass,
                               params: CalculateMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("CalculateMapping", propertyName, parentMapping, params)
  }

  def createGeocoordinatesMapping(propertyName: String, parentMapping: OWLClass,
                             params: GeocoordinatesMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("GeocoordinatesMapping", propertyName, parentMapping, params)
  }

  def createConstantMapping(propertyName: String, parentMapping: OWLClass,
                            params: ConstantMappingParameters): OWLNamedIndividual = {
    createPropertyMapping("ConstantMapping", propertyName, parentMapping, params)
  }

  private def createPropertyMapping(propertyClass: String, propertyName: String, parentMapping: OWLClass,
                             params: PropertyMappingParameters): OWLNamedIndividual = {
    val mapping = createOntologyInstance(propertyName, parentMapping,
      MappingSchemaOntology.ontologyClass(propertyClass))
    addParameters(mapping,params)
    mapping
  }

  private def createIRIfromString(name: String): IRI = {
    IRI.create(ontology_iri + "#" + name.replace(" ","_"))
  }

  private def createOntologyClass(name: String, superClasses: List[OWLClass]): OWLClass = {
    createOntologyClass(createIRIfromString(name), superClasses,name)
  }

  private def createOntologyClass(iri: IRI, superClasses: List[OWLClass], name:String): OWLClass = {
    val mapping = factory.getOWLClass(iri)
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(mapping))
    manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(
      factory.getRDFSLabel, iri, factory.getOWLLiteral(name)))
    for ( superClass <- superClasses) {
      manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(mapping, superClass))
    }
    mapping
  }

  private def createOntologyInstance(propertyName: String, parentMapping: OWLClass, mappingType: OWLClass): OWLNamedIndividual = {
    val name = IRI.create(parentMapping.getIRI + "_" + propertyName.replace(" ","_"))
    val mapping = factory.getOWLNamedIndividual(name)
    manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(
      factory.getRDFSLabel, name, factory.getOWLLiteral(propertyName)))
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(mapping))
    manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(parentMapping, mapping))
    manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(mappingType, mapping))
    mapping
  }

  private def addParameters(mapping: OWLNamedObject, params: MappingParameters): Unit = {
    params match {
      case TemplateMappingParameters(mapToClass, correspondingClass, correspondingProperty) =>
        addParameter(mapping, "/templateParameter#mapToClass", mapToClass, true)
        addParameter(mapping, "/templateParameter#correspondingClass", correspondingClass, false)
        addParameter(mapping, "/templateParameter#correspondingProperty", correspondingProperty, false)
      case ConditionMappingParameters(templatePropety, operator, value) =>
        addParameter(mapping, "/conditionParameter#templatePropety", templatePropety, false)
        addParameter(mapping, "/conditionParameter#operator", operator, true)
        addParameter(mapping, "/conditionParameter#value", value, false)
      case IntermediateNodeMappingParameters(nodeClass, correspondingProperty) =>
        addParameter(mapping, "/intermediateNodeParameter#nodeClass", nodeClass, true)
        addParameter(mapping, "/intermediateNodeParameter#correspondingProperty", correspondingProperty, true)
      case SimplePropertyMappingParameters(templateProperty, ontologyProperty, select, prefix, suffix, transform, unit, language, factor) =>
        addParameter(mapping, "/propertyParameter#templatePropety", templateProperty, true)
        addParameter(mapping, "/propertyParameter#ontologyProperty", ontologyProperty, true)
        addParameter(mapping, "/propertyParameter#select", select, false)
        addParameter(mapping, "/propertyParameter#prefix", prefix, false)
        addParameter(mapping, "/propertyParameter#suffix", suffix, false)
        addParameter(mapping, "/propertyParameter#transform", transform, false)
        addParameter(mapping, "/propertyParameter#unit", unit, false)
        addParameter(mapping, "/propertyParameter#language", language, false)
        addParameter(mapping, "/propertyParameter#factor", factor, false)
      case DateIntervalMappingParameters(templateProperty, startDateOntologyProperty, endDateOntologyProperty) =>
        addParameter(mapping, "/dateIntervalParameter#templatePropety", templateProperty, true)
        addParameter(mapping, "/dateIntervalParameter#startDateOntologyProperty", startDateOntologyProperty, true)
        addParameter(mapping, "/dateIntervalParameter#endDateOntologyProperty", endDateOntologyProperty, true)
      case CombineDateMappingParameters(templateProperty1, templateProperty2, templateProperty3, unit1, unit2, unit3, ontologyProperty) =>
        addParameter(mapping, "/combineDateParameter#templateProperty1", templateProperty1, true)
        addParameter(mapping, "/combineDateParameter#templateProperty2", templateProperty2, true)
        addParameter(mapping, "/combineDateParameter#templateProperty3", templateProperty3, false)
        addParameter(mapping, "/combineDateParameter#unit1", unit1, true)
        addParameter(mapping, "/combineDateParameter#unit2", unit2, true)
        addParameter(mapping, "/combineDateParameter#unit3", unit3, templateProperty3.isDefined)
        addParameter(mapping, "/combineDateParameter#ontologyProperty", ontologyProperty, true)
      case CalculateMappingParameters(templateProperty1, templateProperty2, unit1, unit2, operation, ontologyProperty) =>
        addParameter(mapping, "/calculateParameter#templateProperty1", templateProperty1, true)
        addParameter(mapping, "/calculateParameter#templateProperty2", templateProperty2, true)
        addParameter(mapping, "/calculateParameter#unit1", unit1, false)
        addParameter(mapping, "/calculateParameter#unit2", unit2, false)
        addParameter(mapping, "/calculateParameter#operation", operation, true)
        addParameter(mapping, "/calculateParameter#ontologyProperty", ontologyProperty, true)
      case GeocoordinatesMappingParameters(ontologyProperty, coordinates, latitude, longitude, longitudeDegrees, longitudeMinutes,
                                          longitudeSeconds,longitudeDirection, latitudeDegrees, latitudeMinutes,
                                          latitudeSeconds, latitudeDirection) =>
        addParameter(mapping, "/geocoordinateParameter#ontologyProperty", ontologyProperty, false)
        addParameter(mapping, "/geocoordinateParameter#coordinates", coordinates, false)
        addParameter(mapping, "/geocoordinateParameter#latitude", latitude, false)
        addParameter(mapping, "/geocoordinateParameter#longitude", longitude, false)
        addParameter(mapping, "/geocoordinateParameter#longitudeDegrees", longitudeDegrees, false)
        addParameter(mapping, "/geocoordinateParameter#longitudeMinutes", longitudeMinutes, false)
        addParameter(mapping, "/geocoordinateParameter#longitudeSeconds", longitudeSeconds, false)
        addParameter(mapping, "/geocoordinateParameter#longitudeDirection", longitudeDirection, false)
        addParameter(mapping, "/geocoordinateParameter#latitudeDegrees", latitudeDegrees, false)
        addParameter(mapping, "/geocoordinateParameter#latitudeMinutes", latitudeMinutes, false)
        addParameter(mapping, "/geocoordinateParameter#latitudeSeconds", latitudeSeconds, false)
        addParameter(mapping, "/geocoordinateParameter#latitudeDirection", latitudeDirection, false)
      case ConstantMappingParameters(ontologyProperty, value, unit) =>
        addParameter(mapping, "/constantParameter#ontologyProperty", ontologyProperty, true)
        addParameter(mapping, "/constantParameter#value", value, true)
        addParameter(mapping, "/constantParameter#unit", unit, false)
    }
  }

  def getPropertyMappingParameters(parentMapping: OWLClass, property: OWLNamedIndividual): PropertyMappingParameters = {
    property.getTypes(ontology)
      .map(c => c.asInstanceOf[OWLClass].getIRI)
      .filter(_ != parentMapping.getIRI)
      .head.getRemainder.get match {
      case "PropertyMapping" =>
        SimplePropertyMappingParameters(
          getStringParameter(property, "/propertyParameter#templatePropety"),
          getIRIParameter(property, "/propertyParameter#ontologyProperty"),
          getOptionalStringParameter(property, "/propertyParameter#select"),
          getOptionalStringParameter(property, "/propertyParameter#prefix"),
          getOptionalStringParameter(property, "/propertyParameter#suffix"),
          getOptionalStringParameter(property, "/propertyParameter#transform"),
          getOptionalIRIParameter(property, "/propertyParameter#unit"),
          getOptionalStringParameter(property, "/propertyParameter#language"),
          getOptionalDouble(property, "/propertyParameter#factor")
        )
      case "DateIntervalMapping" =>
        DateIntervalMappingParameters(
          getStringParameter(property, "/dateIntervalParameter#templatePropety"),
          getIRIParameter(property, "/dateIntervalParameter#startDateOntologyProperty"),
          getIRIParameter(property, "/dateIntervalParameter#endDateOntologyProperty")
        )
      case "CombineDateMapping" =>
        CombineDateMappingParameters(
          getStringParameter(property,"/combineDateParameter#templateProperty1"),
          getStringParameter(property,"/combineDateParameter#templateProperty2"),
          getOptionalStringParameter(property, "/combineDateParameter#templateProperty3") ,
          getIRIParameter(property,"/combineDateParameter#unit1"),
          getIRIParameter(property, "/combineDateParameter#unit2"),
          getOptionalIRIParameter(property, "/combineDateParameter#unit3"),
          getIRIParameter(property, "/combineDateParameter#ontologyProperty")
        )
      case "CalculateMapping" =>
        CalculateMappingParameters(
          getStringParameter(property,"/calculateParameter#templateProperty1"),
          getStringParameter(property,"/calculateParameter#templateProperty2"),
          getOptionalIRIParameter(property,"/calculateParameter#unit1"),
          getOptionalIRIParameter(property,"/calculateParameter#unit2"),
          getStringParameter(property, "/calculateParameter#operation"),
          getIRIParameter(property, "/calculateParameter#ontologyProperty")
        )

      case "GeocoordinatesMapping" =>
        GeocoordinatesMappingParameters(
          getOptionalIRIParameter(property, "/geocoordinateParameter#ontologyProperty"),
          getOptionalStringParameter(property,"/geocoordinateParameter#coordinates"),
          getOptionalStringParameter(property, "/geocoordinateParameter#latitude"),
          getOptionalStringParameter(property, "/geocoordinateParameter#longitude"),
          getOptionalStringParameter(property, "/geocoordinateParameter#longitudeDegrees"),
          getOptionalStringParameter(property, "/geocoordinateParameter#longitudeMinutes"),
          getOptionalStringParameter(property, "/geocoordinateParameter#longitudeSeconds"),
          getOptionalStringParameter(property, "/geocoordinateParameter#longitudeDirection"),
          getOptionalStringParameter(property, "/geocoordinateParameter#latitudeDegrees"),
          getOptionalStringParameter(property, "/geocoordinateParameter#latitudeMinutes"),
          getOptionalStringParameter(property, "/geocoordinateParameter#latitudeSeconds"),
          getOptionalStringParameter(property, "/geocoordinateParameter#latitudeDirection"))
      case "ConstantMapping" =>
        ConstantMappingParameters(
          getIRIParameter(property, "/constantParameter#ontologyProperty"),
          getStringParameter(property, "/constantParameter#value"),
          getOptionalIRIParameter(property,  "/constantParameter#unit")
        )
      case other =>
        throw new IllegalArgumentException("Unkown Mapping type " + other + " for class: " + parentMapping.getIRI + " at " + property.getIRI)
    }
  }

  private def addParameter(mapping: OWLNamedObject, parameterName: String, annotation: Any, required: Boolean): Unit = {
    val property = MappingSchemaOntology.annotationProperty(parameterName)
    annotation match {
      case Some(some_iri: IRI) =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, some_iri))
      case ontology_iri: IRI =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, ontology_iri))
      case Some(literal: String) =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case literal: String =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case Some(literal: Double) =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case None =>
        if(required) throw new IllegalArgumentException("missing required parameter " + property.getIRI + " for " + mapping.getIRI)
    }
  }

  private def getIRIParameter(entity: OWLEntity, property: String): IRI = {
    getAnnotations(entity, property).map(_.asInstanceOf[IRI]).head
  }

  private def getOptionalIRIParameter(entity: OWLEntity, property: String): Option[IRI] = {
    getAnnotations(entity, property).map(_.asInstanceOf[IRI]).headOption
  }

  private def getStringParameter(entity: OWLEntity, property: String): String = {
    getAnnotations(entity, property).map(_.asInstanceOf[OWLLiteral].getLiteral).head
  }

  private def getOptionalStringParameter(entity: OWLEntity, property: String): Option[String] = {
    getAnnotations(entity, property).map(_.asInstanceOf[OWLLiteral].getLiteral).headOption
  }

  private def getOptionalDouble(entity: OWLEntity, property: String): Option[Double] = {
    getAnnotations(entity, property).map(_.asInstanceOf[OWLLiteral].parseDouble()).headOption
  }

  private def getAnnotations(entity: OWLEntity, parameterName: String): Set[OWLAnnotationValue] = {
    val property = MappingSchemaOntology.annotationProperty(parameterName)
    entity.getAnnotations(ontology,property).map(_.getValue).toSet
  }
}

