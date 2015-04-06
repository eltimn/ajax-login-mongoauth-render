package code.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftmodules.mongoauth.model.SimpleUser
import net.liftweb.http.js.JsCmds.SetHtml

// jQuery('#'+"menu").html("");

class AjaxLogin {
  def render = {
    "#ajax-login" #> ajaxButton("login", () => {
      SimpleUser.logUserIn(SimpleUser.createRecord, true, false)
      println("SimpleUser: "+SimpleUser.isLoggedIn)
      SetHtml("menu", <div data-lift="MenuX.item?name=user">user page</div>)
    })
  }
}
