window.onload = ->
  if logoutTag = document.getElementById 'logout'
    Elm.Logout.embed logoutTag
