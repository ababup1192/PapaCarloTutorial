package org.ababup1192

import name.lakhin.eliah.projects.papacarlo.examples.Json
import name.lakhin.eliah.projects.papacarlo.syntax.Node

// papacarlo.syntax.Node型を少し扱いやすくした型
case class MyNode(id: Int, parentId: Int, childrenId: List[Int], kind: String, value: Option[Any])

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
  }

  /**
   * NodeをIDを使って取得していって、取得出来たらMyNode型に変換してListに追加していく。
   * @return ID順のMyNodeのList。
   */
  def ast: List[MyNode] = {
    val ast = addedNodes.reverse.foldLeft(List.empty[MyNode]) { (list, id) =>
      jsonSyntax.getNode(id) match {
        case Some(node) => exportNode(node) :: list
        case None => list
      }
    }.reverse
    ast
  }

  /**
   * Node -> MyNodeに変換するためのメソッド
   * @param node 変換対象のノード
   * @return 変換後のノード
   */
  private def exportNode(node: Node): MyNode = {
    val parentId = node.getParent.map(_.getId).getOrElse(-1)

    val value = node.getValues.flatMap(_._2).headOption
    val childrenId = node.getBranches.flatMap(_._2).map(_.getId).toList

    MyNode(node.getId, parentId, childrenId, node.getKind, value)
  }

}