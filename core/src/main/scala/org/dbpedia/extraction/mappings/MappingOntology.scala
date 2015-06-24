package org.dbpedia.extraction.mappings

import java.io.File
import java.net.URLEncoder

import org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.{StreamDocumentTarget, SystemOutDocumentTarget, OWLXMLOntologyFormat}
import org.semanticweb.owlapi.model._

import scala.collection.mutable.HashMap


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
case class GeocoordinatesMappingParameters(coordinates: Option[String],
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
    ontologyClassCache.getOrElseUpdate(className,factory.getOWLClass(IRI.create(schema_ontology_iri + "#" + className)))
  }
  
  def annotationProperty(parameterName: String): OWLAnnotationProperty = {
    annotationPropertyCache.getOrElseUpdate(parameterName, factory.getOWLAnnotationProperty(IRI.create(schema_ontology_iri + parameterName)))
  }
}

class MappingOntology(name: String, file: File) {

  val manager = OWLManager.createOWLOntologyManager
  val ontology_iri = IRI.create("http://dbpedia.org/mappings/" + name.replace(" ","_"))
  val ontology = manager.createOntology(ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  def saveOntology(prefixesFrom: OWLOntology): Unit = {
    val owlxmlformat = new OWLXMLOntologyFormat()
    manager.saveOntology(ontology, owlxmlformat ,
      IRI.create(file))
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
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/templateParameter#mapToClass"), mapToClass, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/templateParameter#correspondingClass"), correspondingClass, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/templateParameter#correspondingProperty"), correspondingProperty, false)
      case ConditionMappingParameters(templatePropety, operator, value) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/conditionParameter#templatePropety"), templatePropety, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/conditionParameter#operator"), operator, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/conditionParameter#value"), value, false)
      case IntermediateNodeMappingParameters(nodeClass, correspondingProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/intermediateNodeParameter#nodeClass"), nodeClass, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/intermediateNodeParameter#correspondingProperty"), correspondingProperty, true)
      case SimplePropertyMappingParameters(templateProperty, ontologyProperty, select, prefix, suffix, transform, unit, language, factor) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#templatePropety"), templateProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#ontologyProperty"), ontologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#select"), select, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#prefix"), prefix, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#suffix"), suffix, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#transform"), transform, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#unit"), unit, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#language"), language, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/propertyParameter#factor"), factor, false)
      case DateIntervalMappingParameters(templateProperty, startDateOntologyProperty, endDateOntologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/dateIntervalParameter#templatePropety"), templateProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/dateIntervalParameter#startDateOntologyProperty"), startDateOntologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/dateIntervalParameter#endDateOntologyProperty"), endDateOntologyProperty, true)
      case CombineDateMappingParameters(templateProperty1, templateProperty2, templateProperty3, unit1, unit2, unit3, ontologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#templateProperty1"), templateProperty1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#templateProperty2"), templateProperty2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#templateProperty3"), templateProperty3, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#unit1"), unit1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#unit2"), unit2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#unit3"), unit3, templateProperty3.isDefined)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/combineDateParameter#ontologyProperty"), ontologyProperty, true)
      case CalculateMappingParameters(templateProperty1, templateProperty2, unit1, unit2, operation, ontologyProperty) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#templateProperty1"), templateProperty1, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#templateProperty2"), templateProperty2, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#unit1"), unit1, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#unit2"), unit2, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#operation"), operation, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/calculateParameter#ontologyProperty"), ontologyProperty, true)
      case GeocoordinatesMappingParameters(coordinates, latitude, longitude, longitudeDegrees, longitudeMinutes,
                                          longitudeSeconds,longitudeDirection, latitudeDegrees, latitudeMinutes,
                                          latitudeSeconds, latitudeDirection) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#coordinates"), coordinates, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#latitude"), latitude, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#longitude"), longitude, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#longitudeDegrees"), longitudeDegrees, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#longitudeMinutes"), longitudeMinutes, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#longitudeSeconds"), longitudeSeconds, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#longitudeDirection"), longitudeDirection, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#latitudeDegrees"), latitudeDegrees, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#latitudeMinutes"), latitudeMinutes, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#latitudeSeconds"), latitudeSeconds, false)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/geocoordinateParameter#latitudeDirection"), latitudeDirection, false)
      case ConstantMappingParameters(ontologyProperty, value, unit) =>
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/constantParameter#ontologyProperty"), ontologyProperty, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/constantParameter#value"), value, true)
        addParameter(mapping, MappingSchemaOntology.annotationProperty("/constantParameter#unit"), unit, false)
    }
  }

  private def addParameter(mapping: OWLNamedObject, property: OWLAnnotationProperty, annotation: Any, required: Boolean): Unit = {
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


}

