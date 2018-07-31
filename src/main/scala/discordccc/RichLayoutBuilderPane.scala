package discordccc

import better.files._
import ccc._, ccc.util._
import discordccc.model._
import javafx.beans.binding.Bindings
import javafx.beans.property.{ReadOnlyBooleanWrapper, ReadOnlyStringWrapper, SimpleObjectProperty}
import javafx.geometry.{Pos}
import javafx.scene.control.{Control, Skin, TextField, ColorPicker, Spinner, TableView, TableColumn, Label, cell, Button}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{BorderPane, HBox, Priority, VBox, Pane}
import javafx.scene.paint.Color
import javafx.scene.text.Font
import javafx.util.converter.DefaultStringConverter
import scala.collection.JavaConverters._

class RichLayoutBuilderPaneText extends BaseApplication {
  val rootUserDataDirectory = (File.home/".discorccc").createDirectories()
  val emojioneDir = (rootUserDataDirectory/"emojione").createDirectories()
  
  val emojis = EmojiOne.emojiLookup.map(e => e._1 -> new WeakImage(s"file:${emojioneDir}/${e._2.filename}.png"))
  val imagesCache: collection.mutable.Map[String, WeakImage] = new LruMap[String, WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new WeakImage(k)
    imagesCache(k) = res
    res
  }
  val markdownRenderer = new DiscordMarkdownRenderer(getHostServices, imagesCache, id => None, id => None, id => None)
  override val sceneRoot = new RichLayoutBuilderPane(new ChatTextInput(markdownRenderer, markdownRenderer.nodeFactory, emojis.mapValues(_.get)))
}

class RichLayoutBuilderPane(chatTextInputFactory: ChatTextInput) extends Control {

  val onSubmit = new SimpleObjectProperty[Content.RichLayout => Unit](this, "onSubmit", _ => ())
  
  def getRichLayout(): Content.RichLayout = Content.RichLayout(
    title = Option(Skin.titleTextField.getText).filter(_.nonEmpty),
    description = Option(Skin.descriptionTextArea.textArea.getText).filter(_.nonEmpty).map(Content.Text),
    url = Option(Skin.urlTextField.getText).filter(_.nonEmpty),
    color = Option(Skin.colorChooser.getValue).filter(_ != Color.TRANSPARENT).map(_.toRgb),
    image = Option(Skin.mainImageUrlTextField.getText).filter(_.nonEmpty).map(t => Content.InlinedImage("", t, "")),
    thumbnail = Option(Skin.thumbnailUrlTextField.getText).filter(_.nonEmpty).map(t => Content.InlinedImage("", t, "")),
    author = Some(Content.RichLayout(
        title = Option(Skin.authorTextField.getText).filter(_.nonEmpty),
        image = Option(Skin.authorImageUrlTextField.getText).filter(_.nonEmpty).map(t => Content.InlinedImage("", t, "")),
      )).filter(e => e.title.nonEmpty || e.image.nonEmpty),
    footer = Some(Content.RichLayout(
        title = Option(Skin.footerTextField.getText).filter(_.nonEmpty),
        image = Option(Skin.footerImageTextField.getText).filter(_.nonEmpty).map(t => Content.InlinedImage("", t, "")),
      )).filter(e => e.title.nonEmpty || e.image.nonEmpty),
    fields = Skin.fields.asScala
  )
  
  override protected val createDefaultSkin = Skin
  
  object Skin extends Skin[RichLayoutBuilderPane] {
    override def getSkinnable = RichLayoutBuilderPane.this
    override def dispose = ()
    
    val colorChooser = new ColorPicker(Color.TRANSPARENT)
    val titleTextField = new TextField().modify(_ setPromptText "title")
    val urlTextField = new TextField().modify(_ setPromptText "url")
    val descriptionTextArea = chatTextInputFactory.modify(_.textArea setPromptText "description")
    val thumbnailUrlTextField = new TextField().modify(_ setPromptText "thumbnail url")
    val mainImageUrlTextField = new TextField().modify(_ setPromptText "main image url")
    val authorImageUrlTextField = new TextField().modify(_ setPromptText "author image url", HBox.setHgrow(_, Priority.SOMETIMES))
    val authorTextField = new TextField().modify(_ setPromptText "author", HBox.setHgrow(_, Priority.SOMETIMES))
    val footerTextField = new TextField().modify(_ setPromptText "footer")
    val footerImageTextField = new TextField().modify(_ setPromptText "footer image")
    
    val fieldsSpinner = new Spinner[Int](0, 100, 0, 1)
    val fieldsTable = new TableView[Content.Field]()
    fieldsTable.setEditable(true)
    fieldsTable.setPrefHeight(5.em)
    fieldsTable.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY)
    
    def fields = fieldsTable.getItems
    fieldsTable.getColumns.setAll(
      new TableColumn[Content.Field, String]("name").modify { t =>
        t.setCellFactory(_ => new cell.TextFieldTableCell(new DefaultStringConverter))
        t.setCellValueFactory(e => new ReadOnlyStringWrapper(e.getValue.name))
        t.setOnEditCommit(commit => fields.set(fields.indexOf(commit.getRowValue), commit.getRowValue.copy(name = commit.getNewValue)))
        t.setEditable(true)
      },
      new TableColumn[Content.Field, String]("value").modify{ t =>
        t.setCellFactory(_ => new cell.TextFieldTableCell(new DefaultStringConverter))
        t.setCellValueFactory(e => new ReadOnlyStringWrapper(e.getValue.value.originalText))
        t.setOnEditCommit(commit => fields.set(fields.indexOf(commit.getRowValue), commit.getRowValue.copy(value = Content.Text(commit.getNewValue))))
        t.setEditable(true)
      },
      new TableColumn[Content.Field, java.lang.Boolean]("inlined").modify { t =>
        val w = JfxUtils.computeTextBounds("inlined", Font.getDefault).getWidth * 1.2
        t.setMaxWidth(w)
        t.setMinWidth(w)
        t.setCellFactory(_ => new cell.CheckBoxTableCell())
        t.setCellValueFactory(e => new ReadOnlyBooleanWrapper(e.getValue.inline))
        t.setOnEditCommit(commit => fields.set(fields.indexOf(commit.getRowValue), commit.getRowValue.copy(inline = commit.getNewValue)))
        t.setEditable(true)
      }
    )
    fields.addListener(change => ())
    
    fieldsSpinner.valueProperty.foreach(n => 
      if (n < fields.size) fields.asScala.trimEnd(fields.size - n)
      else if (n > fields.size) fields.addAll(Seq.fill(n - fields.size)(Content.Field("", Content.Text(""), true)):_*) )
    
    val submitButton = new Button("Submit")
    submitButton.setOnAction { _ => 
      onSubmit.get()(getRichLayout())
      colorChooser.setValue(Color.TRANSPARENT)
      authorImageUrlTextField.clear()
      authorTextField.clear()
      titleTextField.clear()
      urlTextField.clear()
      descriptionTextArea.textArea.clear()
      mainImageUrlTextField.clear()
      fieldsSpinner.getValueFactory.setValue(0)
      footerTextField.clear()
      footerImageTextField.clear()
      thumbnailUrlTextField.clear()
    }
    
    //submit is only enabled if at least one of the fields has a value
    submitButton.disableProperty bind Bindings.not(
      Seq(
        authorImageUrlTextField.textProperty.isNotEmpty,
        authorTextField.textProperty.isNotEmpty,
        titleTextField.textProperty.isNotEmpty,
        urlTextField.textProperty.isNotEmpty,
        descriptionTextArea.textArea.textProperty.isNotEmpty,
        mainImageUrlTextField.textProperty.isNotEmpty,
        fieldsSpinner.valueProperty.isNotEqualTo(0),
        footerTextField.textProperty.isNotEmpty,
        footerImageTextField.textProperty.isNotEmpty,
        thumbnailUrlTextField.textProperty.isNotEmpty,
      ).reduceOption(Bindings.or).getOrElse(new ReadOnlyBooleanWrapper(false))
    )
    
    
    override val getNode = hbox(
      {
        val res = new BorderPane()
        res.setTop(hbox(
            colorChooser,
            imageViewFor(authorImageUrlTextField, 1.5.em),
            authorImageUrlTextField,
            authorTextField,
          )(alignment = Pos.CENTER_LEFT, spacing = 0.2.em))
        
        res.setCenter(vbox(
            titleTextField,
            urlTextField,
            descriptionTextArea,
            mainImageUrlTextField,
            imageViewFor(mainImageUrlTextField, 15.em),
            hbox(new Label("fields"), fieldsSpinner),
            fieldsTable.modify(VBox.setVgrow(_, Priority.SOMETIMES))
          )(alignment = Pos.TOP_CENTER, fillWidth = true, spacing = 0.2.em))
        
        res.setBottom(hbox(
            footerTextField,
            footerImageTextField,
            imageViewFor(footerImageTextField, 1.5.em),
          )(alignment = Pos.CENTER_LEFT, spacing = 0.2.em))
        
        res
      },
      vbox(
        thumbnailUrlTextField,
        imageViewFor(thumbnailUrlTextField, 5.em),
        new Pane().modify(VBox.setVgrow(_, Priority.ALWAYS)), //filler
        submitButton
      )(alignment = Pos.TOP_CENTER, fillWidth = true)
    )(alignment = Pos.CENTER_LEFT, fillHeight = true)
    
    private def imageViewFor(textField: TextField, width: Double) = {
      val res = new ImageView()
      res.setPreserveRatio(true)
      res.setFitWidth(width)
      textField.textProperty foreach (text => 
        try if (text.isEmpty) res.setImage(null) else res.setImage(new Image(text))
        catch { case e: IllegalArgumentException =>}
      )
      res
    }
    
  }
}
