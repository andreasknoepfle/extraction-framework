package org.dbpedia.extraction.util

import org.dbpedia.extraction.mappings.MappingOntology
import org.dbpedia.extraction.wikiparser.{TemplateNode, Node, PageNode}


class MappingOwlConverter(page: PageNode, node: Node, mappingOntology: MappingOntology) {
  val tnode = node.asInstanceOf[TemplateNode]
  val name = page.title.decoded

  def convertTemplateMapping(tnode: TemplateNode): Unit = {
    val test = tnode.property("mapToClass")
    test.hashCode()
  }

  def convertConditionalMapping(tnode: TemplateNode): Unit = {

  }

  def convert: Unit = {
    tnode.title.decoded match {
      case "TemplateMapping" => {
        convertTemplateMapping(tnode)
      }
      case "TableMapping" => {
        // ignore this because it is used only one and is also deprecated
      }
      case "ConditionalMapping" => {
        convertConditionalMapping(tnode)
      }
      case _ => throw new scala.IllegalArgumentException("Unknown mapping element " + tnode.title.decoded)
    }
  }

}
