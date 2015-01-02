do ($ = jQuery, scope = window) ->
  $(document).ready ->
    $(".get-action-logs").each (index)->
      panel_wrapper = $(this)
      panel = panel_wrapper.find(".panel")
      url = panel_wrapper.data("url")
      $.getJSON(url)
        .done((json)->
          if json.length
            $.each json, (i, log)->
              username = log.username
              username = $("<a></a>").attr("href", log.userUrl).text(username) if log.userUrl
              p = $("<p></p>")
              p.append(log.message + " - ").append(username).append(", " + log.timeAgo)
              panel.find(".panel-body").append(p)
            panel_wrapper.removeClass("hidden")
          else
            panel_wrapper.remove()
        )
        .fail((jqxhr, textStatus, error)->
          panel_wrapper.remove()
        )
