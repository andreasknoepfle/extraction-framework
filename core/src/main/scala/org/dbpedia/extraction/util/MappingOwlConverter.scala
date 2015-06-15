package org.dbpedia.extraction.util

import org.dbpedia.extraction.mappings.MappingOntology
import org.dbpedia.extraction.wikiparser.{TemplateNode, Node, PageNode}
import org.semanticweb.owlapi.model.{OWLClass, OWLProperty}


class MappingOwlConverter(page: PageNode, node: Node, mappingOntology: MappingOntology) {
  val tnode = node.asInstanceOf[TemplateNode]
  val name = page.title.decoded

  def convertTemplateMapping(tnode: TemplateNode, name: String): Unit = {
    val mapToClass = fetchOntologyClass(tnode,"mapToClass")
    val correspondingClass = fetchOntologyClass(tnode,"correspondingClass")
    val correspondingProperty = fetchOntologyProperty(tnode,"correspondingProperty")
    val templateMapping = mappingOntology.createTemplateMapping(name, mapToClass, correspondingClass, correspondingProperty)
    extractPropertyMappings(tnode, templateMapping)
  }

  def convertConditionalMapping(tnode: TemplateNode, name: String): Unit = {

  }

  def extractPropertyMappings(tnode: TemplateNode, mapping: OWLClass ): Unit = {

  }

  def fetchOntologyClass(tnode: TemplateNode, attribute: String): OWLClass = {

  }

  def fetchOntologyProperty(tnode: TemplateNode, attribute: String): OWLProperty = {

  }

  def convert: Unit = {
    tnode.title.decoded match {
      case "TemplateMapping" => {
        convertTemplateMapping(tnode,name)
      }
      case "TableMapping" => {
        // ignore this because it is used only one and is also deprecated
      }
      case "ConditionalMapping" => {
        convertConditionalMapping(tnode,name)
      }
      case _ => throw new scala.IllegalArgumentException("Unknown mapping element " + tnode.title.decoded)
    }
  }

}
