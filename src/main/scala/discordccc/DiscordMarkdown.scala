package discordccc

import discordccc.model._

object DiscordMarkdown {

  /**
   * Apply necessary transformations to the Discord supported markdown to transform it to standard markdown.
   */
  def adaptToMarkdown(message: Message): String = {
    val res = new StringBuilder
    message.content foreach {
      case Content.Text(t) => res.append(t)
      case Content.InlinedImage(name, url, _, true) => res.append(s"""![emoji]($url "name")""")
      case Content.InlinedImage(name, url, _, _) => res.append(s"""![image]($url "name")""")
      case _ => 
    }
    res.result
  }
}
