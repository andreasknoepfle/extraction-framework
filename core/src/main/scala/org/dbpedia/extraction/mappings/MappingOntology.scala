package org.dbpedia.extraction.mappings

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.mutable.HashMap


class TemplateMappingParameter(mapToClass: OWLClass,
                               correspondingClass: OWLClass,
                               correspondingProperty: OWLProperty)

class ConditionMappingParameter(templateProperty: String,
                                 operator: String,
                                 value: String)

object MappingSchemaOntology {
  val schema_ontology_iri = IRI.create("http://dbpedia.org/mappings/")
  val manager = OWLManager.createOWLOntologyManager
  val ontology = manager.createOntology(schema_ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  val cache = new HashMap[String,OWLClass]

  def classFromAnchor(anchor: String): OWLClass = {
    cache.getOrElseUpdate(anchor,factory.getOWLClass(IRI.create(schema_ontology_iri + "#" + anchor)))
  }
}

class MappingOntology(name: String, file: File) {
  val manager = OWLManager.createOWLOntologyManager
  val ontology_iri = IRI.create("http://dbpedia.org/mappings/" + name)
  val ontology = manager.createOntology(ontology_iri)
  val factory = ontology.getOWLOntologyManager().getOWLDataFactory()

  def createTemplateMapping(name: String, params: TemplateMappingParameter) :OWLClass = {
    val superClasses = List (MappingSchemaOntology.classFromAnchor("DBpediaMapping"),
      MappingSchemaOntology.classFromAnchor("TemplateMapping"))
    createClass(name, superClasses)
  }

  def createConditionalMapping(name: String): OWLClass = {
    createClass(
      name,
      List(MappingSchemaOntology.classFromAnchor("DBpediaMapping"),
        MappingSchemaOntology.classFromAnchor("ConditionalMapping"))
    )
  }

  def createConditionMapping(name: String, conditionalMapping: OWLClass, params: ConditionMappingParameter): OWLClass = {
    createClass(
      name,
      List(conditionalMapping,
        MappingSchemaOntology.classFromAnchor("TemplateMapping"))
    )
  }

  private def createIRIfromString(name: String): IRI = {
    IRI.create(ontology_iri + "#" + name)
  }

  private def createClass(name: String, superClasses: List[OWLClass]): OWLClass = {
    val owl_class = factory.getOWLClass(createIRIfromString(name))
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(owl_class))
    for ( superClass <- superClasses) {
      manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(owl_class, superClass))
    }
    owl_class
  }

  private def addAnnotation(property: OWLAnnotationProperty , subject: OWLAnnotationSubject ,  value: OWLAnnotationValue): Unit = {
    manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(property, subject, value))
  }

}

