package org.dbpedia.extraction.dump.owlextract

import org.dbpedia.extraction.mappings.{Disambiguations, Mappings, Redirects}
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.owlmappings.{MappingOntology,OWLPrefixConverter}
import org.dbpedia.extraction.sources.{Source, WikiPage}
import org.dbpedia.extraction.util.Language

/**
 * TODO: remove this class. Different extractors need different resources. We should use some kind
 * of dependency injection (not necessarily a framework, Scala should be flexible enough). That
 * would also make configuration much easier and more flexible. No more loading of classes by name.
 * 
 * Problems with the current approach:
 * - unflexible
 * - we lose static type safety because of 
 *   - reflection when the extractor objects are created
 *   - structural types in extractor constructors
 */
trait DumpExtractionContextOWL
{
    def ontology : Ontology

    // TODO: remove this, only used by ImageExtractor
    def commonsSource : Source

    def language : Language

    // TODO: remove this, only used by MappingExtractor
    def mappingOntology : MappingOntology

    // TODO: remove this, only used by MappingExtractor
    def mappings : Mappings

    def articlesSource : Source

    def redirects : Redirects

    def disambiguations : Disambiguations

    def prefixConverter : OWLPrefixConverter
}

