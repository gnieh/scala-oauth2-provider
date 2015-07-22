package scalaoauth2.directives

import scalaoauth2.provider._

import spray.routing.Directive1
import spray.routing.directives._

import spray.http.{
  FormData,
  MultipartFormData
}

trait OAuth2Directives extends BasicDirectives  with ParameterDirectives with MarshallingDirectives {

  val protectedResource: ProtectedResource = ProtectedResource

  val tokenEndpoint: TokenEndpoint = TokenEndpoint

  def allFormFields: Directive1[Map[String, List[String]]] =
    entity(as[FormData]).map {
      case FormData(fields) =>
        fields.foldLeft(Map.empty[String, List[String]]) {
          case (acc, (key, value)) =>
            acc.updated(key, value :: acc.getOrElse(key, Nil))
        }
    } |
    entity(as[MultipartFormData]).map {
      case MultipartFormData(fields) =>
        fields.foldLeft(Map.empty[String, List[String]]) {
          case (acc, part) =>
            part.name match {
              case Some(name) =>
                acc.updated(name, part.entity.asString :: acc.getOrElse(name, Nil))
              case None =>
                acc
            }
        }
    } |
    provide(Map.empty[String, List[String]])

  def allParameters: Directive1[Map[String, List[String]]] =
    for {
      formFields <- allFormFields
      queryParams <- parameterMultiMap
    } yield formFields ++ queryParams

}
