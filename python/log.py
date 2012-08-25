
g_file = None

def set_log_file(file_path):
	global g_file
	g_file = open(file_path, "w")

def info(msg):
	print msg
	if g_file is not None:
		g_file.write(msg + "\n")

def infos(msgs):
	for msg in msgs:
		info(msg)

def error(msg):
	info("[ERROR] " + msg)

def warn(msg):
	info("[WARN]" + msg)