package longevity.integration.model.complexConstraint

import longevity.model.annotations.persistent

@persistent[DomainModel]
case class ComplexConstraint(
  id: ComplexConstraintId,
  primaryEmail: Email,
  emails: Set[Email]) {

  if (!emails.contains(primaryEmail))
    throw new ConstraintValidationException("primary email is not in emails")

}

object ComplexConstraint {
  lazy val keySet = Set(key(ComplexConstraint.props.id))
}
