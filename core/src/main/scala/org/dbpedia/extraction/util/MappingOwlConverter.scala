package org.dbpedia.extraction.util

import org.dbpedia.extraction.dataparser.StringParser
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.wikiparser.{TemplateNode, Node, PageNode}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.DefaultPrefixManager

class MappingOwlConverter(page: PageNode, node: Node, mappingOntology: MappingOntology, ontology: OWLOntology) {
  val rootNode = node.asInstanceOf[TemplateNode]
  val templateName = page.title.decoded
  val prefixManager = ontology.getOWLOntologyManager.getOntologyFormat(ontology).asPrefixOWLOntologyFormat()
  prefixManager.setPrefix("geo","http://dummy.geo")
  prefixManager.setPrefix("skos","http://dummy.skos")
  prefixManager.setPrefix("dct","http://dummy.dct")
  prefixManager.setPrefix("georss","http://dummy.georss")
  prefixManager.setPrefix("bibo","http://dummy.bibo")
  def convert: Unit = {
    rootNode.title.decoded match {
      case "TemplateMapping" => {
        convertTemplateMapping(rootNode,templateName)
      }
      case "TableMapping" => {
        // ignore this because it is used only one and is also deprecated
      }
      case "ConditionalMapping" => {
        convertConditionalMapping(rootNode,templateName)
      }
      case _ => throw new scala.IllegalArgumentException("Unknown mapping element " + rootNode.title.decoded )
    }
  }

  private

  def convertTemplateMapping(tnode: TemplateNode, name: String): Unit = {
    val templateMapping = mappingOntology.createTemplateMapping(name,
      TemplateMappingParameters(
        fetchOntologyIRI(tnode,"mapToClass").get,
        fetchOntologyIRI(tnode,"correspondingClass"),
        fetchOntologyIRI(tnode,"correspondingProperty")))
    extractPropertyMappings(tnode, templateMapping, "mappings")
  }

  def convertConditionalMapping(tnode: TemplateNode, name: String): Unit = {
    val conditionalMapping = mappingOntology.createConditionalMapping(name)

    for( casesProperty <- tnode.property("cases").toList;
         conditionNode <- casesProperty.children if conditionNode.isInstanceOf[TemplateNode]) {
      convertConditionMapping(conditionNode.asInstanceOf[TemplateNode], conditionalMapping, name)
    }
    extractPropertyMappings(tnode, conditionalMapping, "defaultMappings")
  }

  def convertConditionMapping(conditionNode: TemplateNode, conditionalMapping: OWLClass, conditionalName : String) = {
    val templateProperty = fetchTemplateProperty(conditionNode, "templateProperty")
    val operator = fetchTemplateProperty(conditionNode, "operator").get
    val value = fetchTemplateProperty(conditionNode, "value")

    val tnode = conditionNode.property("mapping").flatMap(mappingNode =>
      mappingNode.children.filter(childNode =>
        childNode.isInstanceOf[TemplateNode]).headOption)
      .getOrElse(throw new IllegalArgumentException("Condition does not define a mapping"))
      .asInstanceOf[TemplateNode]
    val name = List(conditionalName,templateProperty.getOrElse("") , operator, value).mkString("_")
    val templateMapping = mappingOntology.createConditionMapping(name, conditionalMapping,
      ConditionMappingParameters(templateProperty,operator,value),
      TemplateMappingParameters(
        fetchOntologyIRI(tnode,"mapToClass").get,
        fetchOntologyIRI(tnode,"correspondingClass"),
        fetchOntologyIRI(tnode,"correspondingProperty")))

    extractPropertyMappings(tnode, templateMapping,"mappings")
  }

  def extractPropertyMappings(tnode: TemplateNode, parentMapping: OWLClass, propertyName: String): Unit = {
    for( mappingsNode <- tnode.property(propertyName);
         mappingNode <- mappingsNode.children if mappingNode.isInstanceOf[TemplateNode]) {
      extractPropertyMapping(mappingNode.asInstanceOf[TemplateNode],parentMapping)
    }
  }

  def extractPropertyMapping(tnode: TemplateNode, parentMapping: OWLClass): Unit = {
    tnode.title.decoded match
    {
      case "PropertyMapping" =>
      {
        val templateProperty = fetchTemplateProperty(tnode,"templateProperty").get
        mappingOntology.createSimplePropertyMapping(
          templateProperty,
          parentMapping,
          SimplePropertyMappingParameters(
            templateProperty,
            fetchOntologyIRI(tnode,"ontologyProperty").get,
            fetchTemplateProperty(tnode,"select"),
            fetchTemplateProperty(tnode,"prefix"),
            fetchTemplateProperty(tnode,"suffix"),
            fetchTemplateProperty(tnode,"transform"),
            fetchOntologyIRI(tnode,"unit"),
            fetchTemplateProperty(tnode,"language"),
            fetchDoubleTemplateProperty(tnode,"factor")
        ))
      }
      case "IntermediateNodeMapping" =>
      {
        val nodeClass = fetchOntologyIRI(tnode,"nodeClass").get
        val mapping = mappingOntology.createIntermediateNodeMapping(
          parentMapping,
          IntermediateNodeMappingParameters(
            nodeClass,
            fetchOntologyIRI(tnode,"correspondingProperty").get
          ))
        extractPropertyMappings(tnode, mapping, "mappings")
      }
      case "DateIntervalMapping" =>
      {
        val templateProperty = fetchTemplateProperty(tnode,"templateProperty").get
        mappingOntology.createDateIntervalMapping(templateProperty, parentMapping,
          DateIntervalMappingParameters(
            templateProperty,
            fetchOntologyIRI(tnode,"startDateOntologyProperty").get,
            fetchOntologyIRI(tnode,"endDateOntologyProperty").get
          ))
      }
      case "CombineDateMapping" =>
      {
        val templateProperty1 = fetchTemplateProperty(tnode,"templateProperty1").get
        val templateProperty2 = fetchTemplateProperty(tnode,"templateProperty2").get
        val templateProperty3 = fetchTemplateProperty(tnode,"templateProperty3")

        val propertyName =  List(templateProperty1, templateProperty2, templateProperty3).mkString("_")

        mappingOntology.createCombineDateMapping(propertyName, parentMapping,
          CombineDateMappingParameters(
            templateProperty1,
            templateProperty2,
            templateProperty3,
            fetchOntologyIRI(tnode,"unit1").get,
            fetchOntologyIRI(tnode,"unit2").get,
            fetchOntologyIRI(tnode,"unit3"),
            fetchOntologyIRI(tnode, "ontologyProperty").get
          ))
      }
      case "CalculateMapping" =>
      {
        val templateProperty1 = fetchTemplateProperty(tnode,"templateProperty1")
        val templateProperty2 = fetchTemplateProperty(tnode,"templateProperty2")
        val propertyName =  List(templateProperty1, templateProperty2).mkString("_")
        mappingOntology.createCalculateMapping(propertyName, parentMapping,
          CalculateMappingParameters(
            templateProperty1.get,
            templateProperty2.get,
            fetchOntologyIRI(tnode,"unit1"),
            fetchOntologyIRI(tnode,"unit2"),
            fetchTemplateProperty(tnode,"operation").get,
            fetchOntologyIRI(tnode, "ontologyProperty").get
          ))



      }
      case "GeocoordinatesMapping" =>
      {
        mappingOntology.createGeocoordinatesMapping(
          "coordinates",
          parentMapping,
          GeocoordinatesMappingParameters(
            fetchTemplateProperty(tnode, "coordinates"),
            fetchTemplateProperty(tnode, "latitude"),
            fetchTemplateProperty(tnode, "longitude"),
            fetchTemplateProperty(tnode, "longitudeDegrees"),
            fetchTemplateProperty(tnode, "longitudeMinutes"),
            fetchTemplateProperty(tnode, "longitudeSeconds"),
            fetchTemplateProperty(tnode, "longitudeDirection"),
            fetchTemplateProperty(tnode, "latitudeDegrees"),
            fetchTemplateProperty(tnode, "latitudeMinutes"),
            fetchTemplateProperty(tnode, "latitudeSeconds"),
            fetchTemplateProperty(tnode, "latitudeDirection")
          ))
      }
      case "ConstantMapping" =>
      {
        val ontologyProperty = fetchOntologyIRI(tnode, "ontologyProperty").get
        mappingOntology.createConstantMapping(
          ontologyProperty.getShortForm,
          parentMapping,
          ConstantMappingParameters(
            ontologyProperty,
            fetchTemplateProperty(tnode, "value").get,
            fetchOntologyIRI(tnode,"unit1")
          )
        )
      }
      case title => throw new IllegalArgumentException("Unknown property type " + title + " on page " + tnode.root.title)
    }
  }


  def fetchDoubleTemplateProperty(tnode: TemplateNode, attribute: String): Option[Double] = {
    fetchTemplateProperty(tnode, attribute) match {
      case Some(str) => Some(str.asInstanceOf[String].toDouble)
      case None => None
    }
  }

  def fetchOntologyIRI(tnode: TemplateNode, attribute: String): Option[IRI] = {
    fetchTemplateProperty(tnode,attribute) match {
      case Some(str) => Some(prefixManager.getIRI(str))
      case None => None
    }
  }

  def fetchTemplateProperty(tnode: TemplateNode, attribute: String): Option[String] = {
    tnode.property(attribute).flatMap(propertyNode => StringParser.parse(propertyNode))
  }



}
