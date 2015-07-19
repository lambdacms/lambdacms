do ($ = jQuery, scope = window)->
  $(document).ready ->
    $(".get-action-logs").each (index)->
      panel_wrapper = $(this)
      panel_body = panel_wrapper.find(".panel .panel-body")
      url = panel_wrapper.data("url")
      chunk_length = %{toJSON actionLogChunkLength}
      load_activities(url, chunk_length - 1, panel_body,
      ->
        panel_wrapper.removeClass("hidden")
        return
      ,
      ->
        panel_wrapper.remove()
        return
      )
      return
    return

  load_activities = (url, amount, panel_body, success_callback, fail_callback)->
    $.getJSON(url + "&offset=" + panel_body.find("> p").length)
      .done((json)->
        if json.length
          $.each json, (i, log)->
            if ( i == amount)
              btn = $("<button></button>",
                type: "button"
                class: "btn btn-primary btn-xs"
                text: panel_body.data("more")
                click: ->
                  $(this).remove()
                  load_activities(url, amount, panel_body)
                  return
              )
              panel_body.append(btn)
            else
              username = log.username
              username = $("<a></a>").attr("href", log.userUrl).text(username) if log.userUrl
              p = $("<p></p>")
              p.append(log.message + " - ").append(username).append(", " + log.timeAgo)
              panel_body.append(p)
          success_callback() if success_callback
        else
          fail_callback() if fail_callback
        return
      )
      .fail((jqxhr, textStatus, error)->
        fail_callback() if fail_callback
        return
      )
    return
