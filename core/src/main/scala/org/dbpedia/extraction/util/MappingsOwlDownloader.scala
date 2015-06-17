package org.dbpedia.extraction.util

import java.io.File
import java.net.URL
import java.util.logging.Level

import org.dbpedia.extraction.mappings.{MappingOntology, TableMapping}
import org.dbpedia.extraction.sources.WikiSource
import org.dbpedia.extraction.wikiparser._
import org.semanticweb.owlapi.apibinding.OWLManager

object MappingsOwlDownloader {
  val apiUrl = Language.Mappings.apiUri
  val parser = WikiParser.getInstance()
  val manager = OWLManager.createOWLOntologyManager()
  //	    // load the importing ontology
  val DBpediaontology = manager.loadOntologyFromOntologyDocument(new File("/home/andi/git/dbpedia-mappings/dbpedia.owl"));


  def main(args: Array[String]): Unit = {
    require(args != null && args.length == 1, "expected one argument for mappings target directory")
    val dir = new File(args(0))

    // don't use mkdirs, that often masks mistakes.
    require(dir.isDirectory || dir.mkdir, "directory [" + dir + "] does not exist and cannot be created")

    downloadMappings(dir)
  }

  def downloadMappings(dir: File): Unit = {
    Namespace.mappings.values.par.foreach { namespace =>
      val language = namespace.name(Language.Mappings)
      val file = new File(dir, language.replace(' ', '_') + ".owl")
      val mappingPageSource = WikiSource.fromNamespaces(Set(namespace), new URL(apiUrl), Language.Mappings)

      val mappingOntology = new MappingOntology(language,file)

      println("Downloading Owl: " + namespace.name(Language.Mappings))
      mappingPageSource.map(parser).flatten.foreach {
        case page => for {node <- page.children if node.isInstanceOf[TemplateNode]} {
          new MappingOwlConverter(page, node, mappingOntology, DBpediaontology ).convert
        }
      }
    }
  }


}