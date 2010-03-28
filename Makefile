check-syntax::
	gcc -S -o /dev/null -gnatwa -gnaty $(CHK_SOURCES) 2>&1 | \
	grep -v 'file name does not match unit name' >&2 || true
