package discordccc

import java.util.Arrays

object RadixTree {

  sealed trait ParentNode {
    protected[RadixTree] var children: Array[ElementNode] = null
  }
  sealed trait ElementNode extends ParentNode {
    private[RadixTree] var parent: ParentNode
    /** @return Common prefix between this node and the passed string.
     *          A positive value that is less than `s.length`indicates that the passed string has remaining
     *          content that should be added to this node (if it is equal to `s.length` it means this node matches the string).
     *          If the value is negative, it means that this node exceeds the passed String and it should be broken up to support
     *          having the passed string as prefix. 
     */
    private[RadixTree] def commonPrefix(s: Array[Byte]): Int
    protected def contentAsString: String
    def length: Int
    final def text = (parent match { case n: ElementNode => n.contentAsString; case _ => "" }) + contentAsString
  }
  @inline private[RadixTree] def println(a: Any): Unit = {
//    Predef.println(a)
  }
  @inline private[RadixTree] def substring(bs: Array[Byte], from: Int): Array[Byte] = {
    val res = new Array[Byte](bs.length - from)
    System.arraycopy(bs, from, res, 0, res.length)
    res
  }
  @inline private[RadixTree] def substring(bs: Array[Byte], from: Int, to: Int): Array[Byte] = {
    val res = new Array[Byte](to - from)
    System.arraycopy(bs, from, res, 0, res.length)
    res
  }
  class Root extends ParentNode {
    def append(s: String): ElementNode = append(s.getBytes)
    def append(t: Array[Byte]): ElementNode = {
      require(t != null && t.nonEmpty, "text cannot be null nor empty")
      def rec(node: ParentNode, t: Array[Byte]): ElementNode = {
        def nodeFor(bytes: Array[Byte]): ElementNode = {
          bytes.length match {
            case 1 => inlinedByteNode(node, bytes(0))
            case 2 => inlinedByteNode(node, bytes(0), bytes(1))
            case 3 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2))
            case 4 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3))
            case 5 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4))
            case 6 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5))
            case 7 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6))
            case 8 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7))
            case 9 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8))
            case 10 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9))
            case 11 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10))
            case 12 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11))
            case 13 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12))
            case 14 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13))
            case 15 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14))
            case 16 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14), bytes(15))
            case 17 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14), bytes(15), bytes(16))
            case 18 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14), bytes(15), bytes(16), bytes(17))
            case 19 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14), bytes(15), bytes(16), bytes(17), bytes(18))
            case 20 => inlinedByteNode(node, bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5), bytes(6), bytes(7), bytes(8), bytes(9), bytes(10), bytes(11), bytes(12), bytes(13), bytes(14), bytes(15), bytes(16), bytes(17), bytes(18), bytes(19))
            case _ => StringNode(node, t)
          }
        }
        def remove(list: Array[ElementNode], elem: ElementNode): Array[ElementNode] =  {
          list.indexOf(elem) match {
            case -1 => list
            case i => 
              val res = new Array[ElementNode](list.length - 1)
              if (i == 0) System.arraycopy(list, 1, res, 0, res.length)
              else {
                System.arraycopy(list, 0, res, 0, i)
                System.arraycopy(list, i + 1, res, i, res.length - i)
              }
              res
          }
        }
          
//          list match {
//          case Nil => Nil
//          case `elem` :: rest => rest
//          case head :: rest => head :: remove(rest, elem)
//        }
        val matchingNodes = if (node.children == null) Iterator.empty else for {
          n <- node.children.iterator
          commonPrefix = n.commonPrefix(t) if commonPrefix != 0
        } yield (n, commonPrefix)
          
        println(s"looking in $node for ${new String(t)}")
        if (matchingNodes.hasNext) {
          val (child, prefix) = matchingNodes.next
          println(s"  found $child @ $prefix")
          prefix match {
            case n if n == t.length => //node already represents the passed string
              println("    node already present")
              child
            case pos if pos > 0 => //string is larger than node, must append
              println("    string is larger than node, must append")
              rec(child, substring(t, pos))
            case neg if neg < 0 => //string is prefix of node, must break it up
              println("    string is prefix of node, must break it up")
              println(s"    removing $child from $node")
              if (node.children != null) node.children = remove(node.children, child)
              val pos = neg.abs
              val (prefix, suffix) = (substring(t, 0, pos), substring(t, pos))
              val (prefixNode, newOldNode) = child match {
                case icn: InlinedByteNode => icn.splitAt(pos)
                  
                case StringNode(_, str) => nodeFor(prefix) -> nodeFor(substring(str, prefix.length))
              }
              node.children :+= prefixNode
              prefixNode.children = Array(newOldNode)
              newOldNode.children = child.children
              child.children = null //allow this list to be reclaimed
              if (newOldNode.children != null) newOldNode.children foreach (_.parent = newOldNode)
              
              rec(prefixNode, suffix)
          }
        } else {
          println("  nothing found, adding new node")
          val res = nodeFor(t)
          if (node.children == null) node.children = Array(res)
          else node.children :+= res
          res
        }
      }
      rec(this, t)
    }
    
    def elementsIterator: Iterator[ElementNode] = {
      def iterator(p: ParentNode): Iterator[ElementNode] = {
        if (p.children == null) Iterator.empty
        else p.children.iterator.flatMap(e => Iterator(e) ++ iterator(e))
      }
      if (children == null) Iterator.empty
      else children.iterator.flatMap(iterator)
    }
    
    override def toString = "Root"
    def toDotString(): String = {
      val sb = new StringBuilder("root [shape=Mdiamond];\n")
      var nodeIdx = -1
      def nextNodeId = {
        nodeIdx += 1
        nodeIdx
      }
      def rec(n: ParentNode, parentLabel: String): Unit = {
        val nodeId = "n" + nextNodeId
        val label = n.toString.replace("\"", "\\\"")
        sb.append(nodeId).append(s""" [label="$label"]""").append(";\n")
        sb.append(parentLabel).append(" -> ").append(nodeId).append(";\n")
        if (n.children != null) n.children foreach (rec(_, nodeId))
      }
      if (children != null) children foreach (rec(_, "root"))
      sb.toString()
    }
  }
  sealed trait InlinedByteNode extends ElementNode {
    protected def myBytes(length: Int): Array[Byte]
    override def toString: String = new String(myBytes(length)).toCharArray.deep.toString
    protected def contentAsString: String = new String(myBytes(length))
    override def commonPrefix(s: Array[Byte]): Int = {
      val myLength = length
      Arrays.mismatch(myBytes(myLength), s) match {
        case -1 => myLength
        case n if n < s.length && n < myLength => -n
        case n => n
      }
    }
    def splitAt(i: Int): (InlinedByteNode, InlinedByteNode) = {
      val l = length
      require(i >= 1, "split position must be greater than 1 ")
      require(i < l, "cannot split at a position past my length " + l)
      val chars = myBytes(l)
      val prefix = i match {
        case 1 => inlinedByteNode(parent, chars(0))
        case 2 => inlinedByteNode(parent, chars(0), chars(1))
        case 3 => inlinedByteNode(parent, chars(0), chars(1), chars(2))
        case 4 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3))
        case 5 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4))
        case 6 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5))
        case 7 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6))
        case 8 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7))
        case 9 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8))
        case 10 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9))
        case 11 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10))
        case 12 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11))
        case 13 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12))
        case 14 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13))
        case 15 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13), chars(14))
        case 16 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13), chars(14), chars(15))
        case 17 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13), chars(14), chars(15), chars(16))
        case 18 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13), chars(14), chars(15), chars(16), chars(17))
        case 19 => inlinedByteNode(parent, chars(0), chars(1), chars(2), chars(3), chars(4), chars(5), chars(6), chars(7), chars(8), chars(9), chars(10), chars(11), chars(12), chars(13), chars(14), chars(15), chars(16), chars(17), chars(18))
      }
      val suffix = l - i match {
        case 1 => inlinedByteNode(prefix, chars(i + 0))
        case 2 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1))
        case 3 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2))
        case 4 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3))
        case 5 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4))
        case 6 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5))
        case 7 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6))
        case 8 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7))
        case 9 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8))
        case 10 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9))
        case 11 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10))
        case 12 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11))
        case 13 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12))
        case 14 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13))
        case 15 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13), chars(i + 14))
        case 16 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13), chars(i + 14), chars(i + 15))
        case 17 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13), chars(i + 14), chars(i + 15), chars(i + 16))
        case 18 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13), chars(i + 14), chars(i + 15), chars(i + 16), chars(i + 17))
        case 19 => inlinedByteNode(prefix, chars(i + 0), chars(i + 1), chars(i + 2), chars(i + 3), chars(i + 4), chars(i + 5), chars(i + 6), chars(i + 7), chars(i + 8), chars(i + 9), chars(i + 10), chars(i + 11), chars(i + 12), chars(i + 13), chars(i + 14), chars(i + 15), chars(i + 16), chars(i + 17), chars(i + 18))
      }
      
      prefix.children = Array(suffix)
      suffix.children = children
      prefix -> suffix
    }
  }
  @inline def inlinedByteNode(parent: ParentNode, b1: Byte, b2: Byte = 0, b3: Byte = 0, b4: Byte = 0, b5: Byte = 0, b6: Byte = 0, b7: Byte = 0, b8: Byte = 0, b9: Byte = 0, b10: Byte = 0, b11: Byte = 0, b12: Byte = 0, b13: Byte = 0, b14: Byte = 0, b15: Byte = 0, b16: Byte = 0, b17: Byte = 0, b18: Byte = 0, b19: Byte = 0, b20: Byte = 0): InlinedByteNode = {
    if (b20 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20)
    else if (b19 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19)
    else if (b18 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18)
    else if (b17 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17)
    else if (b16 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
    else if (b15 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
    else if (b14 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
    else if (b13 != 0) ByteNode20(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
    else if (b12 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
    else if (b11 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
    else if (b10 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
    else if (b9 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7, b8, b9)
    else if (b8 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7, b8)
    else if (b7 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6, b7)
    else if (b6 != 0) ByteNode12(parent, b1, b2, b3, b4, b5, b6)
    else if (b5 != 0) ByteNode12(parent, b1, b2, b3, b4, b5)
    else if (b4 != 0) ByteNode4(parent, b1, b2, b3, b4)
    else if (b3 != 0) ByteNode4(parent, b1, b2, b3)
    else if (b2 != 0) ByteNode4(parent, b1, b2)
    else ByteNode4(parent, b1)
  }
  case class ByteNode4(var parent: ParentNode, b1: Byte, b2: Byte = 0, b3: Byte = 0, b4: Byte = 0) extends InlinedByteNode {
    protected def myBytes(length: Int): Array[Byte] = length match {
      case 1 => Array(b1)
      case 2 => Array(b1, b2)
      case 3 => Array(b1, b2, b3)
      case 4 => Array(b1, b2, b3, b4)
    }
    def length: Int = if (b4 != 0) 4 else if (b3 != 0) 3 else if (b2 != 0) 2 else 1
  }
  case class ByteNode12(var parent: ParentNode, b1: Byte, b2: Byte = 0, b3: Byte = 0, b4: Byte = 0, b5: Byte = 0, b6: Byte = 0, b7: Byte = 0, b8: Byte = 0, b9: Byte = 0, b10: Byte = 0, b11: Byte = 0, b12: Byte = 0) extends InlinedByteNode {
    protected def myBytes(length: Int): Array[Byte] = length match {
      case 1 => Array(b1)
      case 2 => Array(b1, b2)
      case 3 => Array(b1, b2, b3)
      case 4 => Array(b1, b2, b3, b4)
      case 5 => Array(b1, b2, b3, b4, b5)
      case 6 => Array(b1, b2, b3, b4, b5, b6)
      case 7 => Array(b1, b2, b3, b4, b5, b6, b7)
      case 8 => Array(b1, b2, b3, b4, b5, b6, b7, b8)
      case 9 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9)
      case 10 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
      case 11 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
      case 12 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
    }
    def length: Int = if (b6 != 0) 6 else if (b5 != 0) 5 else if (b4 != 0) 4 else if (b3 != 0) 3 else if (b2 != 0) 2 else 1
  }
  case class ByteNode20(var parent: ParentNode, b1: Byte, b2: Byte = 0, b3: Byte = 0, b4: Byte = 0, b5: Byte = 0, b6: Byte = 0, b7: Byte = 0, b8: Byte = 0, b9: Byte = 0, b10: Byte = 0, b11: Byte = 0, b12: Byte = 0, b13: Byte = 0, b14: Byte = 0, b15: Byte = 0, b16: Byte = 0, b17: Byte = 0, b18: Byte = 0, b19: Byte = 0, b20: Byte = 0) extends InlinedByteNode {
    protected def myBytes(length: Int): Array[Byte] = length match {
      case 1 => Array(b1)
      case 2 => Array(b1, b2)
      case 3 => Array(b1, b2, b3)
      case 4 => Array(b1, b2, b3, b4)
      case 5 => Array(b1, b2, b3, b4, b5)
      case 6 => Array(b1, b2, b3, b4, b5, b6)
      case 7 => Array(b1, b2, b3, b4, b5, b6, b7)
      case 8 => Array(b1, b2, b3, b4, b5, b6, b7, b8)
      case 9 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9)
      case 10 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
      case 11 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
      case 12 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
      case 13 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
      case 14 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
      case 15 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
      case 16 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
      case 17 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17)
      case 18 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18)
      case 19 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19)
      case 20 => Array(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20)
    }
    def length: Int = if (b20 != 0) 20 else if (b19 != 0) 19 else if (b18 != 0) 18 else if (b17 != 0) 17 else if (b16 != 0) 16 else if (b15 != 0) 15 else if (b14 != 0) 14 else if (b13 != 0) 13 else if (b12 != 0) 12 else if (b11 != 0) 11 else if (b10 != 0) 10 else if (b9 != 0) 9 else if (b8 != 0) 8 else if (b7 != 0) 7 else if (b6 != 0) 6 else if (b5 != 0) 5 else if (b4 != 0) 4 else if (b3 != 0) 3 else if (b2 != 0) 2 else 1
  }
  case class StringNode(var parent: ParentNode, string: Array[Byte]) extends ElementNode {
    def commonPrefix(s: Array[Byte]): Int = {
      Arrays.mismatch(string, s) match {
        case -1 => string.length
        case n if n < s.length && n < string.length => -n
        case n => n
      }
    }
    override def toString = new String(string)
    override def length = string.length
    protected def contentAsString: String = toString
  }
  
}

object RadixTreeTest {
  def main(args: Array[String]): Unit = {
    val root = new RadixTree.Root()
    root.append("romane")
    root.append("romanus")
    root.append("romulus")
    root.append("rubens")
    root.append("ruber")
    root.append("rubicon")
    root.append("rubicundus")
    root.append("test")
    root.append("slow")
    root.append("water")
    root.append("slower")
    println(root.toDotString)
    
//    println(org.openjdk.jol.info.ClassLayout.parseClass(Class.forName("discordccc.RadixTree$ByteNode4")).toPrintable)
    println(org.openjdk.jol.info.GraphLayout.parseInstance(root).toFootprint)
    println(org.openjdk.jol.info.GraphLayout.parseInstance(Array(
          "romane","romanus", "romulus", "rubens", "ruber", "rubicundus", "test", "slow", "water", "slower"
        )).toFootprint)
    println(org.openjdk.jol.info.GraphLayout.parseInstance(RadixTree.StringNode(null, "12345678".getBytes)).toFootprint)
  }
}
