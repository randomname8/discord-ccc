package discordccc

import discordccc.model._

object DiscordMarkdown {

  /**
   * Apply necessary transformations to the Discord supported markdown to transform it to standard markdown.
   */
  def adaptToMarkdown(message: Message): String = {
    val res = new StringBuilder
    message.content foreach {
      case Content.Text(t) => res.append(t.trim.replace(" ```", "\n```").replace("\n", "\\\n"))
      case Content.InlinedImage(name, url, _, true, w, h) => res.append(s"""![emoji]($url "name")""")
      case Content.InlinedImage(name, url, _, _, w, h) if w != -1 && h != -1 => res.append(s"""![image width=$w;height=$h]($url "name")""")
      case _ => 
    }
    res.result
  }
}
