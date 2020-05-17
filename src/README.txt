
This guide is made for set INCLUDES path in the file ET_VIEWER.ERL.

The only thing that needs to be fixed is the et-xx.yy.zz substring in the includes.
Obviously you need to set this string with your "folder number" that appear in your otp installation.
Follow these steps:

	1)Open et_viewer.erl and go to initials includes,you 'll find its.
	2)Go to folder in the path denoted from $ROOTDIR(open an erl shell,and give os.getenv("ROOTDIR") ).
	3)Then go in the lib/ folder.(i assume you are in $ROOTDIR)
	4)Finally,you have to search a folder with a name like et-xx.yy.zz.
	  After,you have to COPY THE NAME OF THIS FOLDER(et-xx.yy.zz),go on et_viewer.erl,and REPLACE the subpath in includes REPORTED,
	  with your et 'names folder.


