package org.dbpedia.extraction.mappings

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.mutable.HashMap


abstract class MappingParameters
case class TemplateMappingParameters(mapToClass: OWLClass,
                                     correspondingClass: OWLClass,
                                     correspondingProperty: OWLProperty ) extends MappingParameters
case class ConditionMappingParameters(templateProperty: String,
                                      operator: String,
                                      value: String) extends MappingParameters
case class IntermediateNodeMappingParameters(nodeClass: OWLClass,
                                             correspondingProperty: OWLProperty) extends MappingParameters

abstract class PropertyMappingParameters extends MappingParameters
case class SimplePropertyMappingParameters(templateProperty: String,
                                     ontologyProperty: OWLProperty,
                                     select: String,
                                     prefix: String,
                                     suffix: String,
                                     transform: String,
                                     unit: OWLDatatype,
                                     language: String,
                                     factor: Double) extends PropertyMappingParameters
case class DateIntervalMappingParameters(templateProperty: String,
                                         startDateOntologyProperty: OWLProperty,
                                         endDateOntologyProperty: OWLProperty) extends PropertyMappingParameters
case class CombineDateMappingParameters(templateProperty1: String,
                                        templateProperty2: String,
                                        templateProperty3: String,
                                        unit1: OWLDatatype,
                                        unit2: OWLDatatype,
                                        unit3: OWLDatatype,
                                        ontologyProperty: OWLProperty) extends PropertyMappingParameters
case class CalculateMappingParameters(templateProperty1: String,
                                      templateProperty2: String,
                                      unit1: OWLDatatype,
                                      unit2: OWLDatatype,
                                      operation: String,
                                      ontologyProperty: OWLProperty) extends PropertyMappingParameters
case class GeocoordinatesMappingParameters(coordinates: String,
                                           latitude: String,
                                           longitude: String,
                                           longitudeDegrees: String,
                                           longitudeMinutes: String,
                                           longitudeSeconds: String,
                                           longitudeDirection: String,
                                           latitudeDegrees: String,
                                           latitudeMinutes: String,
                                           latitudeSeconds: String,
                                           latitudeDirection: String) extends PropertyMappingParameters
case class ConstantMappingParameters(ontologyProperty: OWLProperty,
                                      value: String,
                                      unit: OWLDatatype) extends PropertyMappingParameters


object MappingSchemaOntology {
  val schema_ontology_iri = IRI.create("http://dbpedia.org/mappings/")
  val manager = OWLManager.createOWLOntologyManager
  val ontology = manager.createOntology(schema_ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  val ontologyClassCache = new HashMap[String, OWLClass]
  val annotationPropertyCache = new HashMap[String, OWLAnnotationProperty]

  def ontologyClass(className: String): OWLClass = {
    ontologyClassCache.getOrElseUpdate(className,factory.getOWLClass(IRI.create(schema_ontology_iri + "#" + className)))
  }
  
  def annotationProperty(parameterName: String): OWLAnnotationProperty = {
    annotationPropertyCache.getOrElseUpdate(parameterName, factory.getOWLAnnotationProperty(IRI.create(schema_ontology_iri + parameterName)))
  }
}

class MappingOntology(name: String, file: File) {

  val manager = OWLManager.createOWLOntologyManager
  val ontology_iri = IRI.create("http://dbpedia.org/mappings/" + name)
  val ontology = manager.createOntology(ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

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
      IRI.create(parentMapping.getIRI + "_" + params.nodeClass.getIRI.getShortForm),
      List(parentMapping,
        MappingSchemaOntology.ontologyClass("PropertyMapping")))
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
    IRI.create(ontology_iri + "#" + name)
  }

  private def createOntologyClass(name: String, superClasses: List[OWLClass]): OWLClass = {
    createOntologyClass(createIRIfromString(name), superClasses)
  }

  private def createOntologyClass(iri: IRI, superClasses: List[OWLClass]): OWLClass = {
    val mapping = factory.getOWLClass(iri)
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(mapping))
    for ( superClass <- superClasses) {
      manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(mapping, superClass))
    }
    mapping
  }

  private def createOntologyInstance(propertyName: String, parentMapping: OWLClass, mappingType: OWLClass): OWLNamedIndividual = {
    val name = IRI.create(parentMapping.getIRI + "_" + propertyName)
    val mapping = factory.getOWLNamedIndividual(name)
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(mapping))
    manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(parentMapping, mapping))
    manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(mappingType, mapping))
  }

  private def addParameters(mapping: OWLNamedObject, params: MappingParameters): Unit = {
    params match {
      case TemplateMappingParameters(mapToClass, correspondingClass, correspondingProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("templateParameter#mapToClass"), mapToClass, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("templateParameter#correspondingClass"), correspondingClass, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("templateParameter#correspondingProperty"), correspondingProperty, false)
      case ConditionMappingParameters(templatePropety, operator, value) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("conditionParameter#templatePropety"), templatePropety, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("conditionParameter#operator"), operator, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("conditionParameter#value"), value, false)
      case IntermediateNodeMappingParameters(nodeClass, correspondingProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("intermediateNodeParameter#nodeClass"), nodeClass, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("intermediateNodeParameter#correspondingProperty"), correspondingProperty, true)
      case SimplePropertyMappingParameters(templateProperty, ontologyProperty, select, prefix, suffix, transform, unit, language, factor) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#templatePropety"), templateProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#ontologyProperty"), ontologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#select"), select, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#prefix"), prefix, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#suffix"), suffix, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#transform"), transform, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#unit"), unit, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#language"), language, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("propertyParameter#factor"), factor, false)
      case DateIntervalMappingParameters(templateProperty, startDateOntologyProperty, endDateOntologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("dateIntervalParameter#templatePropety"), templateProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("dateIntervalParameter#startDateOntologyProperty"), startDateOntologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("dateIntervalParameter#endDateOntologyProperty"), endDateOntologyProperty, true)
      case CombineDateMappingParameters(templateProperty1, templateProperty2, templateProperty3, unit1, unit2, unit3, ontologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#templateProperty1"), templateProperty1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#templateProperty2"), templateProperty2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#templateProperty3"), templateProperty3, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#unit1"), unit1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#unit2"), unit2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#unit3"), unit3, templateProperty3 != null)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("combineDateParameter#ontologyProperty"), ontologyProperty, true)
      case CalculateMappingParameters(templateProperty1, templateProperty2, unit1, unit2, operation, ontologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#templateProperty1"), templateProperty1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#templateProperty2"), templateProperty2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#unit1"), unit1, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#unit2"), unit2, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#operation"), operation, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("calculateParameter#ontologyProperty"), ontologyProperty, true)
      case GeocoordinatesMappingParameters(coordinates, latitude, longitude, longitudeDegrees, longitudeMinutes,
                                          longitudeSeconds,longitudeDirection, latitudeDegrees, latitudeMinutes,
                                          latitudeSeconds, latitudeDirection) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#coordinates"), coordinates, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#latitude"), latitude, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#longitude"), longitude, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#longitudeDegrees"), longitudeDegrees, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#longitudeMinutes"), longitudeMinutes, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#longitudeSeconds"), longitudeSeconds, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#longitudeDirection"), longitudeDirection, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#latitudeDegrees"), latitudeDegrees, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#latitudeMinutes"), latitudeMinutes, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#latitudeSeconds"), latitudeSeconds, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("geocoordinateParameter#latitudeDirection"), latitudeDirection, false)
      case ConstantMappingParameters(ontologyProperty, value, unit) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("constantParameter#ontologyProperty"), ontologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("constantParameter#value"), value, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("constantParameter#unit"), unit, false)
    }
  }

  private def addParameter(mapping: OWLNamedObject, property: OWLAnnotationProperty, annotation: Any, required: Boolean): Unit = {
    annotation match {
      case owlClass: OWLClass =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, owlClass.getIRI))
      case owlProperty: OWLProperty =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, owlProperty.getIRI))
      case owlDatatype: OWLDatatype =>
    manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, owlDatatype.getIRI))
      case literal: String =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case literal: Int =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case literal: String =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, factory.getOWLLiteral(literal)))
      case None =>
        if(required) throw new IllegalArgumentException("missing required parameter " + property.getIRI + " for " + mapping.getIRI)
    }


  }


}

