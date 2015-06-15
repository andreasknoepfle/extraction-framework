package org.dbpedia.extraction.owlmappings

import org.semanticweb.owlapi.model.{IRI, OWLOntology}
import org.semanticweb.owlapi.vocab.PrefixOWLOntologyFormat

class OWLPrefixConverter(ontology: OWLOntology) {

  val prefixManager = ontology.getOWLOntologyManager.getOntologyFormat(ontology).asPrefixOWLOntologyFormat()
  prefixManager.setPrefix("dbodt", "http://dbpedia.org/datatype/")

  def getPrefixManager(): PrefixOWLOntologyFormat = {
    prefixManager
  }

  def getPrefixIRI(iri: IRI): String = {
    val prefixedIRI = prefixManager.getPrefixIRI(iri)
    if (prefixedIRI != null && prefixedIRI.startsWith(":")) {
      iri.getShortForm
    } else {
      prefixedIRI
    }
  }

  def getPrefixIRIorOWLThing(iri: IRI): String = {
    val prefixedIRI = getPrefixIRI(iri)
    if(prefixedIRI == null) {
      "owl:Thing"
    } else {
      prefixedIRI
    }
  }

  def isDBpediaEntity(iri: IRI): Boolean = {
    iri.toString.startsWith(prefixManager.getDefaultPrefix)
  }

}
