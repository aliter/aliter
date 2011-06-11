module NPC
  module Meta
    def new(player, id)
      #self(player, id)
    end

    def name(name)
      set_ivar('name, name)
    end

    def display_name
      "[" + @name + "]"
    end

    def sprite(sprite)
      set_ivar('sprite, sprite)
    end

    def register(info)
      Erlang.gen_server.cast 'zone_master,
        {'register_npc,
          (info['name] || @name).to_list,
          info['sprite] || @sprite,
          info['map],
          info['coordinates],
          info['direction],
          self}
    end

    def buildup(player, id, packets := [])
      receive
      match 'finish
        Erlang.zone_npc.do_all player, packets
        buildup(player, id)
      match x
        buildup(player, id, packets + [x])
      end
    end

    def __bound__(player, id)
      @('player: player,
        'id: id,
        'builder: Process.spawn -> buildup(player, id))
    end
  end

  def __mixed_in__(base)
    base.mixin NPC::Meta
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
      if Erlang.is_function(chosen)
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

  def load(script)
    {'ok, path} = Erlang.application.get_env('aliter, 'npc_path)
    Erlang.elixir.file(path + "/" + script)
  end
end
