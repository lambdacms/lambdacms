do ($ = jQuery, scope = window) ->
  $(document).ready ->
    formToggles = $('.form-toggle')

    formToggles.each (index)->
      glyph = $('<span />').addClass('glyphicon')

      $(this).prepend glyph
      toggleForm this
      return

    formToggles.on 'click', ->
      btn = $(this)

      btn.data('expanded', !btn.data('expanded'))
      toggleForm this
      return
    return

  toggleForm = (button)->
    btn = $(button)
    glyph = btn.find('span.glyphicon')
    form = btn.parent('.form-toggle-wrapper').find('form')

    if btn.data('expanded')
      form.show()
      glyph.addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-right')
    else
      form.hide()
      glyph.addClass('glyphicon-chevron-right').removeClass('glyphicon-chevron-down')
    return
