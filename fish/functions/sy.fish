function sy

    switch (echo $argv[1])
        case "--serve"
            subl ~/siawyoung.github.io
            cd ~/siawyoung.github.io
            bundle exec jekyll serve -w
            cd
        case "--publish"
            set argcount (count $argv)
            echo $argcount
            if math "$argcount < 2" > /dev/null
              echo "Specify a commit message"
              return 1
            end
            rm -rf ~/siawyoung.github.io.publish.backup
            cd ~/siawyoung.github.io
            bundle exec jekyll build
            git add .
            git commit -m "$argv[-1]"
            git push
            cp -fR ~/siawyoung.github.io.publish ~/siawyoung.github.io.publish.backup
            cd ~/siawyoung.github.io.publish/
            rm -r *
            cp -fR ~/siawyoung.github.io/_site/  ~/siawyoung.github.io.publish/
            cd ~/siawyoung.github.io.publish
            git add .
            git commit -m "$argv[-1]"
            git push -f
        case "--new"
            set newdate (date +%Y-%m-%d)
            set categories "[$argv[2]"
            for x in $argv[3..-2]
                set categories $categories","$x
            end
            set categories $categories"]"
            echo -e "---\ntitle:\nlayout: post\ncategories: $categories\ndate: $newdate\n---" > ~/siawyoung.github.io/_posts/$argv[2]/$newdate-$argv[-1].markdown
            subl ~/siawyoung.github.io/_posts/$argv[2]/$newdate-$argv[-1].markdown
        case "--gallery"
            set python_arguments ""
            for x in $argv[3..-2]
                set python_arguments $python_arguments $x
            end
            set path ""
            for x in $argv[3..-2]
                set path $path"/"$x
            end
            python ~/siawyoung.github.io/_plugins/gallery.py $argv[2] $python_arguments $argv[-1] >> ~/siawyoung.github.io/_posts/$argv[3]/$argv[5].markdown
            mv $argv[2] ~/siawyoung.github.io$path
        case ""
            echo -e "\nUsage:\n"
            echo -e "--serve\nOpens the website folder in Sublime Text, and opens a local server at localhost:4000\n"
            echo -e "--publish\nBuilds the Jekyll files from source and copies it to the publish folder. The existing publish folder is backed up in the same directory.\n"
            echo -e "--new [main-category] [nested-categories] [post-name]\nCreates a new post in the [main-category] folder titled [post-name]. The date defaults to today's date, and the categories tag is populated with the [main-category] followed by [nested-categories] if any.\n"
            echo -e "--gallery [folder] [main-category] [nested-categories] [post-name]\nRuns gallery.py on [folder] to generate HTML markup and append to [post-name] (which must exist). Specified folder is then moved to appropriate location.\n"
        case "*"
            echo "Invalid command."
    end
end
