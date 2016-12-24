function gh
  switch (echo $argv[1])
    case ""
      cd ~/github
    case "*"
      cd ~/github/$argv[1]
  end
end