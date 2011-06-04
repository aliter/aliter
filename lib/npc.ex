module NPC
  def initialize(player, id)
    @('player: player, 'id: id, 'builder: Process.spawn -> buildup(player, id))
  end
  
  def buildup(player, id, packets := [])
    receive
    match 'finish
      Erlang.log.error "Done building up packets!", []
      Erlang.zone_npc.do_all player, packets
      buildup(player, id)
    match x
      Erlang.log.error "Building packets up.", [{'packet, x}]
      buildup(player, id, packets + [x])
    end
  end

  def red(text)
    colored("FF0000", text)
  end

  def green(text)
    colored("00FF00", text)
  end

  def blue(text)
    colored("0000FF", text)
  end

  def colored(color, text)
    "^" + color + text + "^000000"
  end

  def say(message)
    Erlang.log.debug "Saying message.", [{ 'message, message }, {'player, @player}, {'id, @id}]
    @builder <- { 'dialog, { @id, message.to_list } }
  end

  def next
    Erlang.log.debug "Sending next button."
    @builder <- { 'dialog_next, @id }
    @builder <- 'finish

    receive 'continue
      Erlang.log.debug "Player clicked next."
    end
  end

  def menu(dict)
    items = dict.to_list
    choices = items.map -> ({k, _}) k.to_list
    @builder <- { 'dialog_menu, { @id, choices } }
    @builder <- 'finish

    receive index
      Erlang.log.debug "Player selected menu item.", [{'index, index}]
      chosen = (items[index - 1])[1]
      if chosen.__parent__ == Function
        chosen.call
      else
        chosen
      end
    end
  end

  def close
    Erlang.log.debug "Sending close button."
    @builder <- { 'dialog_close, @id }
    @builder <- 'finish

    receive 'close
      Erlang.log.debug "Player clicked close."
      Erlang.exit 'normal
    end
  end
end
