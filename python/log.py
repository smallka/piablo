
log_file = None

def set_log_file(file_path):
	global log_file
	log_file = open(file_path, "w")

def info(msg):
	print msg
	if log_file is not None:
		log_file.write(msg + "\n")

def infos(msgs):
	for msg in msgs:
		info(msg)

def error(msg):
	info("[ERROR] " + msg)

def warn(msg):
	info("[WARN]" + msg)