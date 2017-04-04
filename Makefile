VER = 0.2.1.1

html	:
	git pull
	rsync -av ../master/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/doc/tfwh-$(VER)/* ./docs/
	git add .
	git commit -a -m "update"
	git push


