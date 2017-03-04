VER = 0.1.0.0

html	:
	-rmbk
	git pull
	stack haddock
	rsync -av .stack-work/install/x86_64-linux/lts-8.3/8.0.2/doc/tfwh-$(VER)/* ./docs/
	git commit -a -m "update"
	git push


