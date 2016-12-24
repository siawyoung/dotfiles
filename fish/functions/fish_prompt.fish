
set -gx fish_prompt_git_status_added 'added'
set -gx fish_prompt_git_status_modified 'modified'
set -gx fish_prompt_git_status_renamed 'renamed'
set -gx fish_prompt_git_status_copied 'copied’'
set -gx fish_prompt_git_status_deleted 'deleted'
set -gx fish_prompt_git_status_untracked 'untracked'
set -gx fish_prompt_git_status_unmerged 'unmerged'

set -gx fish_prompt_git_status_order added modified renamed copied deleted untracked unmerged

# Without abbreviating directory names
function prompt_pwd_new
    echo $PWD | sed -e "s|^$HOME|~|"
end

function __terlar_git_prompt_new --description 'Write out the git prompt'
  set -l branch (git rev-parse --abbrev-ref HEAD ^/dev/null)
  if test -z $branch
    return
  end

  # ignore home please
  # if $PWD == "~"
  #   return
  # end

  echo -n '('

  set -l index (git status --porcelain ^/dev/null|cut -c 1-2|sort -u)

  set -g git_dirty_count (git status --porcelain  | wc -l | sed "s/ //g")

  # if branch is clean
  if test -z "$index"
    set_color $fish_color_git_clean
    echo -n $branch
    set_color normal
    echo -n ')'
    echo
    return
  end

  set -l gs
  set -l staged

  for i in $index
    if echo $i | grep '^[AMRCD]' >/dev/null
      set staged 1
    end

    switch $i
      case 'A '               ; set gs $gs added
      case 'M ' ' M'          ; set gs $gs modified
      case 'R '               ; set gs $gs renamed
      case 'C '               ; set gs $gs copied
      case 'D ' ' D'          ; set gs $gs deleted
      case '\?\?'             ; set gs $gs untracked
      case 'U*' '*U' 'DD' 'AA'; set gs $gs unmerged
    end
  end

  if set -q staged[1]
    set_color $fish_color_git_staged
  else
    set_color $fish_color_git_dirty
  end

  echo -n $branch

  for i in $fish_prompt_git_status_order
    if contains $i in $gs
      set -l color_name fish_color_git_$i
      set -l status_name fish_prompt_git_status_$i

      set_color $$color_name
      echo -n ' '$$status_name
    end
  end

  echo -n ' '$git_dirty_count

  set_color normal
  echo -n ')'
  echo
end

function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  # User
  set_color $fish_color_user
  echo -n (whoami)
  set_color normal

  echo -n ' at '

  # Host
  set_color $fish_color_host
  echo -n (hostname -s)
  set_color normal

  echo -n ' in '

  # PWD
  set_color $fish_color_cwd
  echo -n (prompt_pwd_new)
  set_color normal

  echo

  __terlar_git_prompt_new

  set_color normal

  echo -n '$ '

end
