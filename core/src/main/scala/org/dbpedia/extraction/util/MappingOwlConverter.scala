package org.dbpedia.extraction.util

import org.dbpedia.extraction.dataparser.StringParser
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.wikiparser.{TemplateNode, Node, PageNode}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.DefaultPrefixManager

class MappingOwlConverter(page: PageNode, node: Node, mappingOntology: MappingOntology, DBpediaOntology: OWLOntology) {
  val rootNode = node.asInstanceOf[TemplateNode]
  val templateName = page.title.decoded
  val manager = OWLManager.createOWLOntologyManager()
  val factory = DBpediaOntology.getOWLOntologyManager().getOWLDataFactory()
  val prefixManager = new DefaultPrefixManager(manager.getOntologyFormat(DBpediaOntology).asPrefixOWLOntologyFormat());

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
      case _ => throw new scala.IllegalArgumentException("Unknown mapping element " + tnode.title.decoded)
    }
  }

  private

  def convertTemplateMapping(tnode: TemplateNode, name: String): Unit = {
    val templateMapping = mappingOntology.createTemplateMapping(name,
      TemplateMappingParameters(
        fetchOntologyClass(tnode,"mapToClass"),
        fetchOntologyClass(tnode,"correspondingClass"),
        fetchOntologyProperty(tnode,"correspondingProperty")))
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
    val operator = fetchTemplateProperty(conditionNode, "operator")
    val value = fetchTemplateProperty(conditionNode, "value"))

    val tnode = conditionNode.property("mapping").asInstanceOf[TemplateNode]
    val name = List(conditionalName,templateProperty , operator, value).mkString("_")
    val templateMapping = mappingOntology.createConditionMapping(name, conditionalMapping,
      ConditionMappingParameters(templateProperty,operator,value),
      TemplateMappingParameters(
        fetchOntologyClass(tnode,"mapToClass"),
        fetchOntologyClass(tnode,"correspondingClass"),
        fetchOntologyProperty(tnode,"correspondingProperty")))

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
        val templateProperty = fetchTemplateProperty(tnode,"templateProperty")
        mappingOntology.createSimplePropertyMapping(
          templateProperty,
          parentMapping,
          SimplePropertyMappingParameters(
            templateProperty,
            fetchOntologyProperty(tnode,"ontologyProperty"),
            fetchTemplateProperty(tnode,"select"),
            fetchTemplateProperty(tnode,"prefix"),
            fetchTemplateProperty(tnode,"suffix"),
            fetchTemplateProperty(tnode,"transform"),
            fetchOntologyDatatype(tnode,"unit"),
            fetchTemplateProperty(tnode,"language"),
            fetchDoubleTemplateProperty(tnode,"factor")
        ))
      }
      case "IntermediateNodeMapping" =>
      {
        val nodeClass = fetchOntologyClass(tnode,"nodeClass")
        val mapping = mappingOntology.createIntermediateNodeMapping(
          parentMapping,
          IntermediateNodeMappingParameters(
            nodeClass,
            fetchOntologyProperty(tnode,"correspondingProperty")
          ))
        extractPropertyMappings(tnode, mapping, "mappings")
      }
      case "DateIntervalMapping" =>
      {
        val templateProperty = fetchTemplateProperty(tnode,"templateProperty")
        mappingOntology.createDateIntervalMapping(templateProperty, parentMapping,
          DateIntervalMappingParameters(
            templateProperty,
            fetchOntologyProperty(tnode,"startDateOntologyProperty"),
            fetchOntologyProperty(tnode,"endDateOntologyProperty")
          ))
      }
      case "CombineDateMapping" =>
      {
        val templateProperty1 = fetchTemplateProperty(tnode,"templateProperty1")
        val templateProperty2 = fetchTemplateProperty(tnode,"templateProperty2")
        val templateProperty3 = fetchTemplateProperty(tnode,"templateProperty3")

        val propertyName =  List(templateProperty1, templateProperty2, templateProperty3).mkString("_")

        mappingOntology.createCombineDateMapping(propertyName, parentMapping,
          CombineDateMappingParameters(
            templateProperty1,
            templateProperty2,
            templateProperty3,
            fetchOntologyDatatype(tnode,"unit1"),
            fetchOntologyDatatype(tnode,"unit2"),
            fetchOntologyDatatype(tnode,"unit3"),
            fetchOntologyProperty(tnode, "ontologyProperty")
          ))
      }
      case "CalculateMapping" =>
      {
        val templateProperty1 = fetchTemplateProperty(tnode,"templateProperty1")
        val templateProperty2 = fetchTemplateProperty(tnode,"templateProperty2")
        val propertyName =  List(templateProperty1, templateProperty2).mkString("_")
        mappingOntology.createCalculateMapping(propertyName, parentMapping,
          CalculateMappingParameters(
            templateProperty1,
            templateProperty2,
            fetchOntologyDatatype(tnode,"unit1"),
            fetchOntologyDatatype(tnode,"unit2"),
            fetchTemplateProperty(tnode,"operation"),
            fetchOntologyProperty(tnode, "ontologyProperty")
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
        val value = fetchTemplateProperty(tnode, "value")
        mappingOntology.createConstantMapping(
          value,
          parentMapping,
          ConstantMappingParameters(
            fetchOntologyProperty(tnode, "ontologyProperty"),
            value,
            fetchOntologyDatatype(tnode,"unit1")
          )
        )
      }
      case title => throw new IllegalArgumentException("Unknown property type " + title + " on page " + tnode.root.title)
    }
  }

  def fetchOntologyClass(tnode: TemplateNode, attribute: String): OWLClass = {
    factory.getOWLClass(fetchOntologyIRI(tnode,attribute))
  }

  def fetchOntologyProperty(tnode: TemplateNode, attribute: String): OWLProperty = {

  }

  def fetchOntologyDatatype(tnode: TemplateNode, attribute: String): OWLDatatype = {

  }

  def fetchDoubleTemplateProperty(tnode: TemplateNode, attribute: String): Double = {

  }

  def fetchOntologyIRI(tnode: TemplateNode, attribute: String): IRI = {
    tnode.property("templateProperty").flatMap(attribute => StringParser.parse(tnode)) match {
      case Some(str) => prefixManager.getIRI(str)
      case None => null
    }
  }

  def fetchTemplateProperty(tnode: TemplateNode, attribute: String): String = {
    tnode.property("templateProperty").flatMap(attribute => StringParser.parse(tnode)) match {
      case Some(str) => str
      case None => null
    }
  }



}
