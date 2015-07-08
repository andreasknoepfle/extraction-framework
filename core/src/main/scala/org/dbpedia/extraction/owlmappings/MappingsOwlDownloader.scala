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
  val ontology = manager.loadOntologyFromOntologyDocument(new File("owlfile.owl"))
  val prefixManager = ontology.getOWLOntologyManager.getOntologyFormat(ontology).asPrefixOWLOntologyFormat()

  // Should be in the Ontology itself
  prefixManager.setPrefix("owl", "http://www.w3.org/2002/07/owl#")
  prefixManager.setPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  prefixManager.setPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
  prefixManager.setPrefix("foaf", "http://xmlns.com/foaf/0.1/")
  prefixManager.setPrefix("geo", "http://www.w3.org/2003/01/geo/wgs84_pos#")
  prefixManager.setPrefix("georss", "http://www.georss.org/georss/")
  prefixManager.setPrefix("gml", "http://www.opengis.net/gml/")
  prefixManager.setPrefix("xsd", "http://www.w3.org/2001/XMLSchema#")
  prefixManager.setPrefix("dc", "http://purl.org/dc/elements/1.1/")
  prefixManager.setPrefix("dct", "http://purl.org/dc/terms/")
  prefixManager.setPrefix("dcterms", "http://purl.org/dc/terms/")
  prefixManager.setPrefix("skos", "http://www.w3.org/2004/02/skos/core#")
  prefixManager.setPrefix("schema", "http://schema.org/")
  prefixManager.setPrefix("bibo", "http://purl.org/ontology/bibo/")
  prefixManager.setPrefix("wikidata", "http://www.wikidata.org/entity/")
  prefixManager.setPrefix("mappings", "http://mappings.dbpedia.org/wiki/")
  prefixManager.setPrefix("d0", "http://www.ontologydesignpatterns.org/ont/d0.owl#")
  prefixManager.setPrefix("dul", "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#")
  prefixManager.setPrefix("cidoccrm", "http://purl.org/NET/cidoc-crm/core#")
  prefixManager.setPrefix("bio", "http://purl.org/vocab/bio/0.1/")

  def main(args: Array[String]): Unit = {
    require(args != null && args.length == 1, "expected one argument for mappings target directory")
    val dir = new File(args(0))

    // don't use mkdirs, that often masks mistakes.
    require(dir.isDirectory || dir.mkdir, "directory [" + dir + "] does not exist and cannot be created")


    downloadMappings(dir, ontology)
  }

  def downloadMappings(dir: File, ontology: OWLOntology): Unit = {
    Namespace.mappings.values.par.foreach { namespace =>
      val language = namespace.name(Language.Mappings)
      val file = new File(dir, language.replace(' ', '_') + ".owl")
      val mappingPageSource = WikiSource.fromNamespaces(Set(namespace), new URL(apiUrl), Language.Mappings)

      val mappingOntology = new MappingOntology(language,file)

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