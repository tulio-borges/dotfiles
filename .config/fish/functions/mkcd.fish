function mkcd
  switch $argv[1]
  case '/*'
    set d $argv[1]
  case '*'
    set d ./$argv[1]
  end
  mkdir -p $d; and cd $d
end

