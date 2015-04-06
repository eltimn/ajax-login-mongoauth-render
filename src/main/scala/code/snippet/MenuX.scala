package code.snippet

import net.liftweb._
import http.{S, DispatchSnippet, LiftRules}
import http.js._
import sitemap._
import util._
import common._
import scala.xml._
import JsCmds._
import JE._
import Helpers._

/**
 * <p>This built-in snippet can be used to render a menu representing your SiteMap.
 * There are three main snippet methods that you can use:</p>
 *
 * <ul>
 *   <li>builder - Renders the entire SiteMap, optionally expanding all child menus</li>
 *   <li>group - Renders the MenuItems corresponding to the specified group.</li>
 *   <li>item - Renders the specific named MenuItem</li>
 * </ul>
 *
 * <p>More detailed usage of each method is provided below</p>
 */
object MenuX extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case "item" => item
  }



  /**
   * <p>Renders a specific, named item, based on the name given in the Menu's Loc paramter:</p>
   *
   * <pre>
   * val menus =
   *   Menu(Loc("a",...,...,LocGroup("test"))) ::
   *   Menu(Loc("b",...,...,LocGroup("test"))) ::
   *   Menu(Loc("c",...,...,LocGroup("test"))) :: Nil
   * </pre>
   *
   * <p>You can then select the item using the name attribute:</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b" /&gt;
   * </pre>
   *
   * <p>The menu item is rendered as an anchor tag (&lta /&gt;). The text for the link
   * defaults to the named Menu's Loc.linkText, but you can specify your own link text
   * by providing contents to the tag:</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b"&gt;This is a link&lt;/lift:Menu.item&gt;
   * </pre>
   *
   * <p>Additionally you can customize
   * the tag using attributes prefixed with "a":</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b" a:style="color: red;" /&gt;
   * </pre>
   *
   * <p>The param attribute may be used with Menu Locs that are
   * CovertableLoc to parameterize the link</p>
   *
   * <p>Normally, the Menu item is not shown on pages that match its Menu's Loc. You can
   * set the "donthide" attribute on the tag to force it to show text only (same text as normal,
   * but not in an anchor tag)</p>
   *
   *
   * <p>Alternatively, you can set the "linkToSelf" attribute to "true" to force a link. You
   * can specify your own link text with the tag's contents. Note that <b>case is significant</b>, so
   * make sure you specify "linkToSelf" and not "linktoself".</p>
   *
   */
  def item(_text: NodeSeq): NodeSeq = {
    val donthide = S.attr("donthide").map(Helpers.toBoolean) openOr false
    val linkToSelf = (S.attr("linkToSelf") or S.attr("linktoself")).map(Helpers.toBoolean) openOr false

    val text = ("a" #> ((n: NodeSeq) => n match {
      case e: Elem => e.child
      case xs => xs
    })).apply(_text)

    for {
      name <- S.attr("name").toList
    } yield {
      type T = Q forSome {type Q}

      println("name: "+name)
      println("findAndTestLoc: "+SiteMap.findAndTestLoc(name))
      println("S.request: "+S.request)
      println("S.request.flatMap(_.location): "+S.request.flatMap(_.location))
      println("""S.attr("param"): """+S.attr("param"))

      // Builds a link for the given loc
      def buildLink[T](loc : Loc[T]) = {
        Group(SiteMap.buildLink(name, text) match {
          case e : Elem =>
            Helpers.addCssClass(loc.cssClassForMenuItem,
                                e % S.prefixedAttrsToMetaData("a"))
          case x => x
        })
      }

      (S.request.flatMap(_.location), S.attr("param"), SiteMap.findAndTestLoc(name)) match {
         case (_, Full(param), Full(loc: Loc[T] with ConvertableLoc[T])) => {
          println("mark 1")
           (for {
             pv <- loc.convert(param)
             link <- loc.createLink(pv)
           } yield
             Helpers.addCssClass(loc.cssClassForMenuItem,
                                 <a href={link}></a> %
                                 S.prefixedAttrsToMetaData("a"))) openOr
           Text("")
         }

         case (Full(loc), _, _) if loc.name == name => {
           println("mark 2")
           (linkToSelf, donthide) match {
             case (true, _) => buildLink(loc)
             case (_, true) => {
               if (!text.isEmpty) {
                 Group(text)
               } else {
                 Group(loc.linkText openOr Text(loc.name))
               }
             }
             case _ => Text("")
           }
         }

         case (Full(loc), _, _) => {
          println("mark 3")
          buildLink(loc)
        }

         case _ => {
          println("mark 4")
          Text("")
        }
       }
    }
  }
}

