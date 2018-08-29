function upload_tmux

  switch (count $argv)
    case 1
      rsync -avzL ~/.tmux.conf root@$argv[1]:~

    case 2
      rsync -avzL ~/.tmux.conf $argv[1]@$argv[2]:~
  end
end