package org.dbpedia.extraction.owlmappings

import java.io.File
import java.net.URL

import org.dbpedia.extraction.sources.WikiSource
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

object MappingsOwlDownloader {
  val apiUrl = Language.Mappings.apiUri
  val parser = WikiParser.getInstance()
  val manager = OWLManager.createOWLOntologyManager()
  //	    // load the importing ontology


  def main(args: Array[String]): Unit = {
    require(args != null && args.length == 2, "USAGE: MappingsOwlDownloader downloadDirectory ontologyFileOWL")
    val dir = new File(args(0))

    // don't use mkdirs, that often masks mistakes.
    require(dir.isDirectory || dir.mkdir, "directory [" + dir + "] does not exist and cannot be created")

    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(1)))
    downloadMappings(dir, ontology)
  }

  def downloadMappings(dir: File, ontology: OWLOntology): Unit = {
    Namespace.mappings.values.par.foreach { namespace =>
      val language = namespace.name(Language.Mappings)
      val file = new File(dir, language.replace(' ', '_') + ".owl")
      val mappingPageSource = WikiSource.fromNamespaces(Set(namespace), new URL(apiUrl), Language.Mappings)
      val mappingOntology = new MappingOntology(language,file)
      val prefixManager = new OWLPrefixConverter(ontology).getPrefixManager()

      println("Downloading Owl: " + namespace.name(Language.Mappings))
      mappingPageSource.map(parser).flatten.foreach {
        case page => for {node <- page.children if node.isInstanceOf[TemplateNode]} {
          try {
            new MappingOwlConverter(page, node, mappingOntology, ontology, prefixManager ).convert
          } catch {
            case e: Exception => System.err.println("Error on page " + namespace.name +
              "/" + page.title.decoded + ": " + e.getMessage)
          }

        }
      }
      println("Converted ... Writing Ontology ...")
      mappingOntology.saveOntology(prefixManager)
      println("Written")

    }
  }


}