package Core

/**
 * @brief Huffman algorithm based encoder. 
 * @param alphabet list of input "characters".
 */
class HuffmanCodec[T](val alphabet : HuffmanCodec.Alphabet[T]) extends BinaryCodec[T] {
  require(!alphabet.isEmpty)
  
  /**
   * Encodes one symbol into byte vector.
   * @param symbol input symbol to be encoded.
   * @return byte vector with encoded data.
   */
  protected def encodeSymbol(symbol : T) : Vector[Byte] = symbolMap(symbol)
  
  /**
   * Decodes byte vector.
   * @param coode byte vector with encoded data.
   * @return decoded data.
   */
  protected def decodeSymbol(code: Vector[Byte]) : (T, Vector[Byte]) =
    (symbolMap.find(p => p._2 == code.take(symbolCodeLength)).get._1, code.drop(symbolCodeLength))
  
  /**
   * @brief Mapping between symbols and their codes.
   */
  private val symbolMap = HuffmanCodec.makeSymbolMap(alphabet)
  
  /**
   * @brief length of each symbol's code.
   */
  private val symbolCodeLength = symbolMap.head._2.length
}

/**
 * @brief HuffmanCodec object.
 */
object HuffmanCodec {
  
  type Alphabet[T] = List[(T, Float)]
  
  def makeAlphabet[T](sample : List[T]) : Alphabet[T] = {
    def go(rest : List[T]) : Map[T, Float] = rest match {
      case Nil => Map()
      case h :: t => {
        val tailMap = go(t)
        if (tailMap contains h) tailMap + ((h, 1.0F)) else tailMap.updated(h, tailMap(h) + 1.0F)
      }
    }
    
    go(sample).toList.map(p => (p._1, p._2 / sample.length.toFloat))
  }
  
  private abstract class HuffmanTree[T](val prob : Float, val sym : List[T])
  private case class Leaf[T](p : Float, s : T) extends HuffmanTree[T](p, List(s))
  private case class Fork[T](val left : HuffmanTree[T], right : HuffmanTree[T])
    extends HuffmanTree[T](left.prob + right.prob, left.sym ::: right.sym)
  
  private def makeSymbolMap[T](alphabet : Alphabet[T]) : Map[T, Vector[Byte]] = {
    val tree = makeHuffmanTree(alphabet)
    
    def go(sym : Alphabet[T]) : Map[T, Vector[Byte]] = sym match {
      case Nil => Map()
      case h :: t => go(t) + ((h._1, buildCode(h._1, tree)))
    }
    
    val symbolMap = go(alphabet)
    val maxLength = symbolMap.maxBy(p => p._2.length)._2.length
    symbolMap.mapValues(_.padTo(maxLength, 0.toByte))
  }
  
  private def makeHuffmanTree[T](alphabet : Alphabet[T]) : HuffmanTree[T] = {
    def go(codes : List[HuffmanTree[T]]) : HuffmanTree[T] = codes match {
      case l :: r :: Nil => Fork(l, r)
      case l :: r :: t => go((Fork(l, r) :: t).sortWith(_.prob < _.prob))
    }
    
    go(alphabet.map(s => Leaf(s._2, s._1)).sortWith(_.prob < _.prob))
  }
  
  private def buildCode[T](sym : T, tree : HuffmanTree[T]) : Vector[Byte] = tree match {
    case Leaf(p, s) => Vector()
    case Fork(l, r) => if (l.sym.contains(sym)) 0.toByte +: buildCode(sym, l) else 1.toByte +: buildCode(sym, r)
  }
}