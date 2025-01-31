package utils.mesh.obj

import utils.math.planar.V2
import utils.math.space.V3
import utils.mesh.obj.Obj.{Face, NamedObjSection, ObjSection, Vertex}

object ObjReader {
  def readFrom(it: Iterator[String])(crushOnError: Boolean = true,debugOutput: Boolean = false): Obj = {
    val res = new Obj
    for (s <- it) {
      processString(s, res)(crushOnError, debugOutput)
    }
    res
  }

  def processString(s: String, obj: Obj)(crushOnError: Boolean = true, debugOutput: Boolean = false): Unit = try {
    val split = s.split(" ")

    split.headOption.foreach {
      case "o" if split.size >= 2 =>
        obj.sections += new NamedObjSection(split(1), new ObjSection())
      case "v" if split.size >= 4 =>
        val vertex = V3(split(1).toDouble, split(2).toDouble, split(3).toDouble)
        obj.vertices += vertex
      case "vt" if split.size >= 3 =>
        val uv = V2(split(1).toDouble, split(2).toDouble)
        obj.uvs += uv
      case "vn" =>
        val normal = V3(split(1).toDouble, split(2).toDouble, split(3).toDouble)
        obj.normals += normal
      case "f" if split.size >= 4 =>
        val face = new Face()
        split.tail.foreach{ vertexText =>
          val splitVertex = vertexText.split("/")
          val vertex = Vertex(splitVertex(0).toInt - 1, splitVertex(1).toInt - 1, splitVertex(2).toInt - 1)
          face.vs += vertex
        }
        obj.sections.last.obj.faces += face
      case s if s.startsWith("#") => /*comment; do nothing**/
      case _ => /*not supported*/
    }
  } catch {
    case e: Exception =>
      if (debugOutput) {
        println(s"Failed to process line: $s")
        e.printStackTrace()
      }
      if(crushOnError)
        throw new ParseLineException(s, e)
  }

  class ParseLineException(line: String, exception: Throwable) extends Exception(s"Failed to parse line: $line", exception)

}
