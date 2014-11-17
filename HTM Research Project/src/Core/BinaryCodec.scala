package Core

/**
 * @brief Interface for binary codecs to convert different input
 * data into byte vector.
 * @param alphabet list of input "characters".
 */
abstract class BinaryCodec[T] {
  
  /**
   * Encodes input data into byte vector.
   * @param data list of input symbols.
   * @return byte vector with encoded data.
   */
  def encode(data : List[T]) : Vector[Byte] = data.map(encodeSymbol(_)).reduce(_ ++ _)
  
  /**
   * Decodes byte vector.
   * @param coode byte vector with encoded data.
   * @return decoded data.
   */
  def decode(code : Vector[Byte]) : List[T] =
    if (code.isEmpty)
      Nil
    else {
      val (symbol, rest) = decodeSymbol(code)
      symbol :: decode(rest)
    }
  
  /**
   * Encodes one symbol into byte vector.
   * @param symbol input symbol to be encoded.
   * @return byte vector with encoded data.
   */
  protected def encodeSymbol(symbol : T) : Vector[Byte]
  
  /**
   * Decodes byte vector.
   * @param coode byte vector with encoded data.
   * @return decoded data.
   */
  protected def decodeSymbol(code: Vector[Byte]) : (T, Vector[Byte])
}