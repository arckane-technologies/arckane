
package arckane.db

package object utils {

  /** Type class that extends string methods for utility. */
  implicit class StringUtils (str: String) {

    /** Removes spaces and new line characters at the beginning and end of the string. */
    def trim: String =
      str.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "").replaceAll("""(\s)+""", " ")

    /** Removes all characters that are not letters, numbers, spaces, interrogation
      * and exclamation symbols. */
    def clean: String =
      str.replaceAll("""[^a-zA-Z0-9()\s?!¿¡.,:áéíóúÁÉÍÓÚ]""", "")

    /** Removes characters */
    def clean (cleaning: String): String = cleaning match {
      case "name" => str.clean
      case "invitation" => str.replaceAll("""[^a-z0-9-]""", "")
      case "none" => str
      case _ => str
    }

    /** Adds backslashes to parenthesis to escape them for regexp searches. */
    def escapeParenthesis: String =
      str.replaceAll("""\(""", """\\(""")
         .replaceAll("""\)""", """\\)""")

    /** Capitalizes each word of the string. */
    def capitalizeWords: String =
      str.split(" ").foldLeft("")(_ + " " + _.capitalize).tail
  }
}
