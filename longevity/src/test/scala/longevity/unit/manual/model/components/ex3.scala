package longevity.unit.manual.model.components.ex3

import longevity.model.annotations.domainModel
@domainModel trait DomainModel

// end prelude

import longevity.model.annotations.component
import longevity.model.annotations.persistent

@component
case class Email(email: String)

@component
case class EmailPreferences(
  primaryEmail: Email,
  emails: Set[Email])

@component
case class Address(
  street: String,
  city: String)

@persistent[DomainModel](keySet = emptyKeySet)
case class User(
  username: String,
  emails: EmailPreferences,
  addresses: Set[Address])
