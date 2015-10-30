package org.ababup1192

import name.lakhin.eliah.projects.papacarlo.examples.Json
import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.syntax.Node
import spray.json._

// papacarlo.syntax.Node型を少し扱いやすくした型
case class MyFragment(id: Int, from: Map[String, Int], to: Map[String, Int])

object JsonParserSimulator {

  // 追加したノードのID 初期値rootノード
  private[this] var addedNodes = List(1)
  // private[this] var removedNodes = List.empty[Int]

  private[this] val jsonLexer = Json.lexer
  private[this] val jsonSyntax = Json.syntax(jsonLexer)

  def main(args: Array[String]): Unit = {
    // ノードが作られるごとにIDをListに追加。
    jsonSyntax.onNodeCreate.bind { node =>
      addedNodes = node.getId :: addedNodes
    }

    // パース対象のJson
    val json =
      """{"foo": {"bar": [1, 2, [3, 4], 4]}, "hoge": true}"""
    jsonLexer.input(json)

    println(json)
    println(ast)
    println(ast.elements.map { element =>
      element.asJsObject.getFields("childrenId")
    })
  }

  /**
   * AST -> JsonASTに変換
   * @return ノードの配列
   */
  def ast: JsArray = {
    val ast = addedNodes.reverse.foldLeft(JsArray.empty) { (jsArray, id) =>
      jsonSyntax.getNode(id) match {
        case Some(node) => JsArray(jsArray.elements :+ exportJsValue(node))
        case None => jsArray
      }
    }
    ast
  }

  /**
   * Node -> JsValueに変換するためのメソッド
   * @param node 変換対象のノード
   * @return 変換後のJsValue
   */
  private def exportJsValue(node: Node): JsValue = {
    val parentId = node.getParent.map(_.getId).getOrElse(-1)

    val childrenId = node.getBranches.flatMap(_._2).map(_.getId).map(JsNumber(_)).toVector
    val values = node.getValues.flatMap(_._2).map(JsString(_)).toVector

    JsObject(Map("id" -> JsNumber(node.getId), "parentId" -> JsNumber(parentId),
      "childrenId" -> JsArray(childrenId), "kind" -> JsString(node.getKind), "value" -> JsArray(values)))
  }

  def getNodeFragment(id: Int): Option[MyFragment] = {
    jsonSyntax.getNode(id) match {
      case Some(node) =>
        Some(MyFragment(id, tokenCursor(node.getBegin), tokenCursor(node.getEnd, after = true)))
      case None => None
    }
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false): Map[String, Int] = {
    val pair = token.collection.cursor(token.index + (if (after) 1 else 0))

    Map("line" -> (pair._1 - 1), "ch" -> (pair._2 - 1))
  }


}