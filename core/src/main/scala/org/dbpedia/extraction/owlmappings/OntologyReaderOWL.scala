package org.dbpedia.extraction.owlmappings

import java.io.File
import java.util.logging.Logger

import org.dbpedia.extraction.ontology._
import org.dbpedia.extraction.ontology.datatypes.{Datatype, DimensionDatatype, UnitDatatype}
import org.dbpedia.extraction.util.Language
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions.asScalaSet



class OntologyReaderOWL(source : File) {

  private val manager = OWLManager.createOWLOntologyManager()
  private val factory = manager.getOWLDataFactory()
  private val logger = Logger.getLogger(classOf[OntologyReaderOWL].getName)
  private val owlOntology = manager.loadOntologyFromOntologyDocument(source)
  private val prefixManager = manager.getOntologyFormat(owlOntology).asPrefixOWLOntologyFormat()

  def read(): Ontology = {
    logger.info("Loading ontology")

    val ontologyBuilder = new OntologyBuilder()

    ontologyBuilder.datatypes = OntologyDatatypes.load()

    ontologyBuilder.classes ::= new ClassBuilder("owl:Thing", Map(Language.Mappings -> "Thing"), Map(Language.Mappings -> "Base class of all ontology classes"), List(), Set(), Set())
    ontologyBuilder.classes ::= new ClassBuilder("rdf:Property", Map(Language.Mappings -> "Property"), Map(), List("owl:Thing"), Set(), Set())

    // TODO: range should be rdfs:Class
    ontologyBuilder.properties ::= new PropertyBuilder("rdf:type", Map(Language.Mappings -> "has type"), Map(), true, false, "owl:Thing", "owl:Thing", Set())
    ontologyBuilder.properties ::= new PropertyBuilder("rdfs:label", Map(Language.Mappings -> "has label"), Map(), false, false, "owl:Thing", "rdf:langString", Set())
    ontologyBuilder.properties ::= new PropertyBuilder("rdfs:comment", Map(Language.Mappings -> "has comment"), Map(), false, false, "owl:Thing", "rdf:langString", Set())

    for ( cls <- owlOntology.getClassesInSignature) {
      ontologyBuilder.classes ::=  new ClassBuilder(prefixManager.getPrefixIRI(cls.getIRI()),
        labels = readAnnotationsByLanguage(cls, "rdfs:label") ++ readAnnotationsByLanguage(cls, "label"),
        comments = readAnnotationsByLanguage(cls, "rdfs:comment") ++ readAnnotationsByLanguage(cls, "comment"),
        baseClassNames = convertClassesToStringList(cls.getSuperClasses(owlOntology)).toList,
        equivClassNames = convertClassesToStringList(cls.getEquivalentClasses(owlOntology)),
        disjClassNames = convertClassesToStringList(cls.getDisjointClasses(owlOntology)))
    }


    val properties = owlOntology.getDataPropertiesInSignature ++ owlOntology.getObjectPropertiesInSignature
    for ( prop <- properties) {

    }

    ontologyBuilder.build()
  }

  private def readAnnotationsByLanguage(cls: OWLClass, prop: String): Map[Language,String] =  {
    val property = factory.getOWLAnnotationProperty(prop, prefixManager)
   cls.getAnnotations(owlOntology, property).map(_.getValue.asInstanceOf[OWLLiteral])
     .map{ literal => (Language.get(literal.getLang).get -> literal.getLiteral ) }
     .toMap[Language,String]
  }

  private def convertClassesToStringList(classList: java.util.Set[OWLClassExpression]): Set[String] = {
    classList.map{ baseClass => prefixManager.getPrefixIRI(baseClass.asOWLClass().getIRI)}.toSet[String]
  }


  private class OntologyBuilder
  {
    var classes = List[ClassBuilder]()
    var properties = List[PropertyBuilder]()
    var datatypes = List[Datatype]()
    var specializedProperties = List[SpecificPropertyBuilder]()
    var equivalentPropertiesBuilderMap = Map[String,Set[PropertyBuilder]] ()        //[wikidataprop,Set[DBpediaeq props]]
  var equivalentClassesMap = Map[String,Set[ClassBuilder]] ()           //[wikidataclass,Set[DBpediaeq class]]

    def build() : Ontology  =
    {
      val classMap = classes.map( clazz => (clazz.name, clazz) ).toMap
      val propertyMap = properties.map( property => (property.name, property) ).toMap
      val typeMap = datatypes.map( datatype => (datatype.name, datatype) ).toMap

      new Ontology( classes.flatMap(_.build(classMap)).map(c => (c.name, c)).toMap,
        properties.flatMap(_.build(classMap, typeMap)).map(p => (p.name, p)).toMap,
        datatypes.map(t => (t.name, t)).toMap,
        specializedProperties.flatMap(_.build(classMap, propertyMap, typeMap)).toMap,
        equivalentPropertiesBuilderMap.map{m=>m._1 -> m._2.flatMap(_.build(classMap,typeMap))},
        equivalentClassesMap.map{m=>m._1 -> m._2.flatMap(_.build(classMap))})
    }
  }

  private class ClassBuilder(val name : String, val labels : Map[Language, String], val comments : Map[Language, String],
                             var baseClassNames : List[String], val equivClassNames : Set[String], val disjClassNames : Set[String])
  {
    require(name != null, "name != null")
    require(labels != null, "labels != null")
    require(comments != null, "comments != null")
    if (name != "owl:Thing" && baseClassNames.isEmpty) baseClassNames = List("owl:Thing")
    require(equivClassNames != null, "equivClassNames != null")

    /** Caches the class, which has been build by this builder. */
    var generatedClass : Option[OntologyClass] = None

    /** Remembers if build has already been called on this object */
    private var buildCalled = false

    def build(classMap : Map[String, ClassBuilder]) : Option[OntologyClass] =
    {
      if(!buildCalled)
      {
        //TODO check for cycles to avoid infinite recursion
        val baseClasses = baseClassNames.map{ baseClassName => classMap.get(baseClassName) match
        {
          case Some(baseClassBuilder) => baseClassBuilder.build(classMap)
          case None if ! RdfNamespace.validate(baseClassName) =>
          {
            logger.config("base class '"+baseClassName+"' of class '"+name+"' was not found, but for its namespace this was expected")
            Some(new OntologyClass(baseClassName, Map(), Map(), List(), Set(), Set()))
          }
          case None =>
          {
            logger.warning("base class '"+baseClassName+"' of class '"+name+"' not found")
            None
          }
        }}.flatten

        val equivClasses = equivClassNames.map{ equivClassName => classMap.get(equivClassName) match
        {
          case Some(equivClassBuilder) => equivClassBuilder.build(classMap)
          case None if ! RdfNamespace.validate(equivClassName) =>
          {
            logger.config("equivalent class '"+equivClassName+"' of class '"+name+"' was not found, but for its namespace this was expected")
            Some(new OntologyClass(equivClassName, Map(), Map(), List(), Set(), Set()))
          }
          case None =>
          {
            logger.warning("equivalent class '"+equivClassName+"' of class '"+name+"' not found")
            None
          }
        }}.flatten

        val disjointClasses = disjClassNames.map{ disjClassNames => classMap.get(disjClassNames) match
        {
          case Some(equivClassBuilder) => equivClassBuilder.build(classMap)
          case None if ! RdfNamespace.validate(disjClassNames) =>
          {
            logger.config("equivalent class '"+disjClassNames+"' of class '"+name+"' was not found, but for its namespace this was expected")
            Some(new OntologyClass(disjClassNames, Map(), Map(), List(), Set(), Set()))
          }
          case None =>
          {
            logger.warning("equivalent class '"+disjClassNames+"' of class '"+name+"' not found")
            None
          }
        }}.flatten

        name match
        {
          case "owl:Thing" => generatedClass = Some(new OntologyClass(name, labels, comments, List(), equivClasses, disjointClasses))
          case _ => generatedClass = Some(new OntologyClass(name, labels, comments, baseClasses, equivClasses, disjointClasses))
        }

        buildCalled = true

      }

      generatedClass
    }
  }

  private class PropertyBuilder(val name : String, val labels : Map[Language, String], val comments : Map[Language, String],
                                val isObjectProperty : Boolean, val isFunctional : Boolean, val domain : String, val range : String,
                                val equivPropertyNames : Set[String], val superPropertyNames : Set[String] = Set())
  {
    require(name != null, "name != null")
    require(labels != null, "labels != null")
    require(comments != null, "comments != null")
    require(domain != null, "domain != null")
    require(range != null, "range != null")
    require(equivPropertyNames != null, "equivPropertyNames != null")
    require(superPropertyNames != null, "superPropertyNames != null")

    /** Caches the property, which has been build by this builder. */
    var generatedProperty : Option[OntologyProperty] = None

    def build(classMap : Map[String, ClassBuilder], typeMap : Map[String, Datatype]) : Option[OntologyProperty] =
    {
      val domainClass = classMap.get(domain) match
      {
        case Some(domainClassBuilder) => domainClassBuilder.generatedClass match
        {
          case Some(cls) => cls
          case None => logger.warning("domain '"+domain+"' of property '"+name+"' could not be loaded"); return None
        }
        // TODO: do we want this? Maybe we should disallow external domain types.
        case None if ! RdfNamespace.validate(domain) =>
        {
          logger.config("domain '"+domain+"' of property '"+name+"' was not found, but for its namespace this was expected")
          new OntologyClass(domain, Map(), Map(), List(), Set(), Set())
        }
        case None => logger.warning("domain '"+domain+"' of property '"+name+"' not found"); return None
      }

      var equivProperties = Set.empty[OntologyProperty]
      for (name <- equivPropertyNames) {
        // FIXME: handle equivalent properties in namespaces that we validate
        if (RdfNamespace.validate(name)) logger.warning("Cannot use equivalent property '"+name+"'")
        else equivProperties += new OntologyProperty(name, Map(), Map(), null, null, false, Set(), Set())
      }

      var superProperties = Set.empty[OntologyProperty]
      for (name <- superPropertyNames) {
        // FIXME: handle equivalent properties in namespaces that we validate
        if (RdfNamespace.validate(name)) logger.warning("Cannot use super property '"+name+"'")
        else superProperties += new OntologyProperty(name, Map(), Map(), null, null, false, Set(), Set())
      }

      if(isObjectProperty)
      {
        val rangeClass = classMap.get(range) match
        {
          case Some(rangeClassBuilder) => rangeClassBuilder.generatedClass match
          {
            case Some(clazz) => clazz
            case None => logger.warning("range '"+range+"' of property '"+name+"' could not be loaded"); return None
          }
          // TODO: do we want this? Maybe we should disallow external range types.
          case None if ! RdfNamespace.validate(range) =>
          {
            logger.config("range '"+range+"' of property '"+name+"' was not found, but for its namespace this was expected")
            new OntologyClass(range, Map(), Map(), List(), Set(), Set())
          }
          case None => logger.warning("range '"+range+"' of property '"+name+"' not found"); return None
        }

        generatedProperty = Some(new OntologyObjectProperty(name, labels, comments, domainClass, rangeClass, isFunctional, equivProperties, superProperties))
      }
      else
      {
        val rangeType = typeMap.get(range) match
        {
          case Some(datatype) => datatype
          case None => logger.warning("range '"+range+"' of property '"+name+"' not found"); return None
        }

        generatedProperty = Some(new OntologyDatatypeProperty(name, labels, comments, domainClass, rangeType, isFunctional, equivProperties, superProperties))
      }

      generatedProperty
    }
  }

  private class SpecificPropertyBuilder(val className : String, val propertyName : String, val datatypeName : String)
  {
    require(className != null, "className != null")
    require(propertyName != null, "propertyName != null")
    require(datatypeName != null, "datatypeName != null")

    def build( classMap : Map[String, ClassBuilder],
               propertyMap : Map[String, PropertyBuilder],
               typeMap : Map[String, Datatype] ) : Option[((OntologyClass, OntologyProperty), UnitDatatype)] =
    {
      //Load the domain class of the property
      val domainClass = classMap.get(className) match
      {
        case Some(domainClassBuilder) => domainClassBuilder.generatedClass match
        {
          case Some(clazz) => clazz
          case None => logger.warning("Cannot specialize property on class '" + className + "', since the class failed to load"); return None
        }
        case None => logger.warning("Cannot specialize property on class '" + className + "', since the class has not been found"); return None
      }

      //Load the base property
      val baseProperty = propertyMap.get(propertyName) match
      {
        case Some(propertyBuilder) => propertyBuilder.generatedProperty match
        {
          case Some(property) => property
          case None => logger.warning("Cannot specialize property '" + propertyName + "' on class '" + className + "', since the property failed to load"); return None
        }
        case None => logger.warning("Cannot specialize property '" + propertyName + "' on class '" + className + "', since the property has not been found"); return None
      }

      //Load the specialized range of the property
      val specializedRange = typeMap.get(datatypeName) match
      {
        case Some(datatype) => datatype
        case None => logger.warning("Cannot specialize property " + propertyName + " on class " + className + ", " +
          "since the range '" + datatypeName + "' has not been found"); return None
      }

      //Check if the range of the base property is a dimension
      if(!baseProperty.range.isInstanceOf[DimensionDatatype])
      {
        logger.warning("Cannot specialize property " + propertyName + " on class " + className + ", " +
          "since the range of the base property '" + baseProperty.range + "' is not a dimension")
        return None
      }

      //Check if the range of the specialized property is a unit
      if(!specializedRange.isInstanceOf[UnitDatatype])
      {
        logger.warning("Cannot specialize property " + propertyName + " on class " + className + ", " +
          "since the range '" + specializedRange + "' is not a unit")
        return None
      }

      //Check if the range of the specialized property is in the dimension of the base property range
      if(specializedRange.asInstanceOf[UnitDatatype].dimension != baseProperty.range)
      {
        logger.warning("Cannot specialize property " + propertyName + " on class " + className + ", " +
          "since the range of the base property has another dimension")
        return None
      }

      Some((domainClass, baseProperty), specializedRange.asInstanceOf[UnitDatatype])
    }
  }

}


