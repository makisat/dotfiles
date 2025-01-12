if status is-interactive
    # Commands to run in interactive sessions can go here
    oh-my-posh init fish --config ~/.config/fish/themes/amro.omp.json | source

    # --- aliases --- #
    # neovim
    alias vi="vim"
    # battery
    alias bat="upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage"

    # --- setups --- #
    # Set up fzf key bindings
    fzf --fish | source

    # --- path --- #
    # go
    fish_add_path /home/makisat/go/bin

end

function fish_greeting
    if test "$TERM" = "alacritty"
	nerdfetch
    end
end
