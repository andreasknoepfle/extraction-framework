package org.dbpedia.extraction.owlmappings

import java.io.File
import java.util.logging.Logger

import org.dbpedia.extraction.ontology._
import org.dbpedia.extraction.ontology.datatypes.{Datatype, DimensionDatatype, UnitDatatype}
import org.dbpedia.extraction.ontology.io.OntologyReader
import org.dbpedia.extraction.util.Language
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions.asScalaSet



class OntologyReaderOWL(owlOntology : OWLOntology) extends OntologyReader {

  private val manager = owlOntology.getOWLOntologyManager()
  private val factory = manager.getOWLDataFactory()
  private val logger = Logger.getLogger(classOf[OntologyReaderOWL].getName)
  private val prefixManager = new OWLPrefixConverter(owlOntology)
  private val datatypes = OntologyDatatypes.load()
  private val datatypeMap = datatypes.map( datatype => (datatype.name, datatype) ).toMap

  def read(): Ontology = {
    logger.info("Loading ontology")

    val ontologyBuilder = new OntologyBuilder()

    ontologyBuilder.datatypes = datatypes

    ontologyBuilder.classes ::= new ClassBuilder("owl:Thing", Map(Language.Mappings -> "Thing"), Map(Language.Mappings -> "Base class of all ontology classes"), List(), Set(), Set())
    ontologyBuilder.classes ::= new ClassBuilder("rdf:Property", Map(Language.Mappings -> "Property"), Map(), List("owl:Thing"), Set(), Set())

    // TODO: range should be rdfs:Class
    ontologyBuilder.properties ::= new PropertyBuilder("rdf:type", Map(Language.Mappings -> "has type"), Map(), true, false, "owl:Thing", "owl:Thing", Set())
    ontologyBuilder.properties ::= new PropertyBuilder("rdfs:label", Map(Language.Mappings -> "has label"), Map(), false, false, "owl:Thing", "rdf:langString", Set())
    ontologyBuilder.properties ::= new PropertyBuilder("rdfs:comment", Map(Language.Mappings -> "has comment"), Map(), false, false, "owl:Thing", "rdf:langString", Set())

    // Import Classes from signature
    for ( cls <- owlOntology.getClassesInSignature) {
      val name = prefixManager.getPrefixIRI(cls.getIRI())
      if(name != null) {
        buildClass(ontologyBuilder, cls, name)
        updateEquivalentClasses(ontologyBuilder, name)
      }
    }
    for ( prop <- owlOntology.getObjectPropertiesInSignature) {

      val name = prefixManager.getPrefixIRI(prop.getIRI())

      val domain = prop.getDomains(owlOntology).headOption match {
        case Some(cls) => prefixManager.getPrefixIRIorOWLThing(cls.asOWLClass().getIRI)
        case None => "owl:Thing"
      }
      if(name != null) { // The property has a dbpedia prefix short form
        val range = prop.getRanges(owlOntology).headOption match {
          case Some(cls) => prefixManager.getPrefixIRIorOWLThing(cls.asOWLClass().getIRI)
          case None => "owl:Thing"
        }
        val isDBpediaDatatype = range.startsWith("dbodt:")
        val property = new PropertyBuilder(
          name,
          readAnnotationsByLanguage(prop, "rdfs:label") ++ readAnnotationsByLanguage(prop, "label"),
          readAnnotationsByLanguage(prop, "rdfs:comment") ++ readAnnotationsByLanguage(prop, "comment"),
          !isDBpediaDatatype && !datatypeMap.contains(range), // if it is a dbpedia datatype property, create a datatypeproperty
          prop.isFunctional(owlOntology),
          domain,
          if(isDBpediaDatatype) cutDatatypePrefix(range) else range,
          convertObjectProperties(prop.getEquivalentProperties(owlOntology)),
          convertObjectProperties(prop.getSuperProperties(owlOntology)))
        ontologyBuilder.properties ::= property
        updateEquivalentProperties(ontologyBuilder, property)
      } else if (isSpecificProperty(prop)) {
        // Specific properties do not generate a short form
        ontologyBuilder.specializedProperties ::= new SpecificPropertyBuilder(
          domain,
          prop.getIRI.getShortForm,
          prop.getRanges(owlOntology).head.asOWLClass().getIRI().getShortForm()
        )
      }
    }

    for(prop <- owlOntology.getDataPropertiesInSignature ) {
      if (!isSpecificProperty(prop) && !isObjectProperty(prop.getIRI)) {
        val datatype = prop.getRanges(owlOntology).headOption
        if(datatype.isEmpty) {
          logger.warning(prop.getIRI + " - Cannot load datatype property " + prop.getIRI  + " because it does not define its range")
        } else {
          var property = new PropertyBuilder(
            prefixManager.getPrefixIRI(prop.getIRI()),
            readAnnotationsByLanguage(prop, "rdfs:label") ++ readAnnotationsByLanguage(prop, "label"),
            readAnnotationsByLanguage(prop, "rdfs:comment") ++ readAnnotationsByLanguage(prop, "comment"),
            false,
            isFunctional = prop.isFunctional(owlOntology),
            prop.getDomains(owlOntology).headOption match {
              case Some(cls) => prefixManager.getPrefixIRIorOWLThing(cls.asOWLClass().getIRI)
              case None => "owl:Thing"
            },
            prefixManager.getPrefixIRI(datatype.get.asOWLDatatype().getIRI),
            convertDataProperties(prop.getEquivalentProperties(owlOntology)),
            convertDataProperties(prop.getSuperProperties(owlOntology)))
          ontologyBuilder.properties ::= property
          updateEquivalentProperties(ontologyBuilder, property)
        }
      }
    }

    ontologyBuilder.build()
  }

  def isObjectProperty(iri: IRI): Boolean = {
    owlOntology.containsObjectPropertyInSignature(iri)
  }

  def cutDatatypePrefix(dt: String): String = {
    dt.drop(6)
  }

  def updateEquivalentProperties(ontologyBuilder: OntologyBuilder, property: PropertyBuilder): Unit = {
    property.equivPropertyNames.foreach { prop =>
      if (prop.contains("wikidata:")) {

        //search for wikidataprop in the equivalentPropertiesBuilderMap keys if exists add DBprop to the set of DBprop
        // if not create new key
        ontologyBuilder.equivalentPropertiesBuilderMap.find { map => map._1 == prop } match {

          case Some(map) => ontologyBuilder.equivalentPropertiesBuilderMap.updated(map._1, property)
          case None => {
            ontologyBuilder.equivalentPropertiesBuilderMap += prop -> Set(property)
          }
        }

      }
    }
  }

  def buildClass(ontologyBuilder: OntologyBuilder, cls: OWLClass, name: String): Unit = {
    ontologyBuilder.classes ::= new ClassBuilder(
      name,
      labels = readAnnotationsByLanguage(cls, "rdfs:label") ++ readAnnotationsByLanguage(cls, "label"),
      comments = readAnnotationsByLanguage(cls, "rdfs:comment") ++ readAnnotationsByLanguage(cls, "comment"),
      baseClassNames = convertClasses(cls.getSuperClasses(owlOntology)).toList,
      Set(),//equivClassNames = convertClasses(cls.getEquivalentClasses(owlOntology)),
      Set())//disjClassNames = convertClasses(cls.getDisjointClasses(owlOntology)))
  }

  def updateEquivalentClasses(ontologyBuilder: OntologyBuilder, name: String): Unit = {
    for (everyClass <- ontologyBuilder.classes) {
      everyClass.equivClassNames.foreach {
        cls =>
          if (cls.contains("wikidata:")) {
            val wikidataClassName = cls
            ontologyBuilder.equivalentClassesMap.find { map => map._1 == name } match {
              case Some(map) => ontologyBuilder.equivalentClassesMap.updated(map._1, everyClass)
              case None => {
                ontologyBuilder.equivalentClassesMap += wikidataClassName -> Set(everyClass)
              }
            }
          }
      }
    }
  }

  private def readAnnotationsByLanguage(cls: OWLEntity, prop: String): Map[Language,String] =  {
    val property = factory.getOWLAnnotationProperty(prop, prefixManager.getPrefixManager())
   cls.getAnnotations(owlOntology, property).map(_.getValue.asInstanceOf[OWLLiteral])
     .map{ literal => (Language.get(literal.getLang).get -> literal.getLiteral ) }
     .toMap[Language,String]
  }

  private def convertClasses(classList: java.util.Set[OWLClassExpression]): Set[String] = {
    classList.map{ baseClass => prefixManager.getPrefixIRI(baseClass.asOWLClass().getIRI)}.filter(_ != null).toSet[String]
  }

  private def convertObjectProperties(propertyList: java.util.Set[OWLObjectPropertyExpression]): Set[String] = {
    propertyList.map{ baseClass => prefixManager.getPrefixIRI(baseClass.asOWLObjectProperty().getIRI)}.filter(_ != null).toSet[String]
  }

  private def convertDataProperties(propertyList: java.util.Set[OWLDataPropertyExpression]): Set[String] = {
    propertyList.map{ baseClass => prefixManager.getPrefixIRI(baseClass.asOWLDataProperty().getIRI)}.filter(_ != null).toSet[String]
  }

  // A specific property is detected when its IRI is nested in a Classes IRI
  // Example: http://dbpedia.org/ontology/Planet/apoapsis
  // Check if http://dbpedia.org/ontology/Planet is a prefixable IRI that starts
  // with the default iri
  private def isSpecificProperty(property: OWLObjectProperty): Boolean = {
    isDBpediaNamespace(property.getIRI().getNamespace())
  }

  private def isSpecificProperty(property: OWLDataProperty): Boolean = {
    isDBpediaNamespace(property.getIRI().getNamespace())
  }

  private def isDBpediaNamespace(_namespace: String): Boolean = {
    val namespace = if(_namespace.endsWith("/")) _namespace.dropRight(1) else _namespace
    prefixManager.getPrefixIRI(IRI.create(namespace)) != null &&
      namespace.startsWith(prefixManager.getPrefixManager().getDefaultPrefix)
  }





}


