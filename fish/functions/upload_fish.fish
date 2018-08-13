function upload_fish

  switch (count $argv)
    case 1
      rsync -avz ~/.config/fish/functions/fish_prompt.fish root@$argv[1]:~/.config/fish/functions/

    case 2
      rsync -avz ~/.config/fish/functions/fish_prompt.fish $argv[1]@$argv[2]:~/.config/fish/functions/
  end
end
