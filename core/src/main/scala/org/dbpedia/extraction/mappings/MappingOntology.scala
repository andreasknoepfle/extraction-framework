package org.dbpedia.extraction.mappings

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.mutable.HashMap


abstract class MappingParameters
case class TemplateMappingParameters(mapToClass: OWLClass, correspondingClass: OWLClass = None, correspondingProperty: OWLProperty = None) extends MappingParameters
case class ConditionMappingParameters(templateProperty: String, operator: String, value: String = None) extends MappingParameters


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

    val mapping = createMapping(name, superClasses)
    addParameters(mapping, params)
    mapping
  }

  def createConditionalMapping(name: String): OWLClass = {
    createMapping(
      name,
      List(MappingSchemaOntology.ontologyClass("DBpediaMapping"),
        MappingSchemaOntology.ontologyClass("ConditionalMapping"))
    )
  }

  def createConditionMapping(name: String, conditionalMapping: OWLClass, params: ConditionMappingParameters): OWLClass = {
    createMapping(
      name,
      List(conditionalMapping,
        MappingSchemaOntology.ontologyClass("TemplateMapping"))
    )
  }


  private def createIRIfromString(name: String): IRI = {
    IRI.create(ontology_iri + "#" + name)
  }

  private def createMapping(name: String, superClasses: List[OWLClass]): OWLClass = {
    val mapping = factory.getOWLClass(createIRIfromString(name))
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(mapping))
    for ( superClass <- superClasses) {
      manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(mapping, superClass))
    }
    mapping
  }

  private def addParameters(mapping: OWLEntity, params: MappingParameters): Unit = {
    params match {
      case TemplateMappingParameters(mapToClass, correspondingClass, correspondingProperty) =>
        addParameter(mapping,MappingSchemaOntology.annotationProperty("templateParameter#mapToClass") , mapToClass, true)
        addParameter(mapping,MappingSchemaOntology.annotationProperty("templateParameter#correspondingClass") , correspondingClass, false)
        addParameter(mapping,MappingSchemaOntology.annotationProperty("templateParameter#correspondingProperty") , correspondingProperty, false)
      case ConditionMappingParameters(templatePropety, operator, value) =>
        addParameter(mapping,MappingSchemaOntology.annotationProperty("conditionParameter#templatePropety") , templatePropety, true)
        addParameter(mapping,MappingSchemaOntology.annotationProperty("conditionParameter#operator") , operator, true)
        addParameter(mapping,MappingSchemaOntology.annotationProperty("conditionParameter#value") , value, false)
    }
  }



  private def addParameter(mapping: OWLEntity, property: OWLAnnotationProperty, annotation: Any, required: Boolean): Unit = {
    annotation match {
      case owlClass: OWLClass =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, owlClass.getIRI))
      case owlProperty: OWLProperty =>
        manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, mapping.getIRI, owlProperty.getIRI))
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

