import xml.dom.minidom

g_opcodes = {}
g_descs = {}
g_log = None

def load_xml(xml_file_path):
	dom = xml.dom.minidom.parse(xml_file_path)
	root = dom.getElementsByTagName("TypeDescriptors")[0]

	print "loading xml..."

	kinds = set()
	for child in root.childNodes:
		if child.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
			kinds.add(child.localName)

	for kind in kinds:
		print "    ", kind, len(root.getElementsByTagName(kind))

	for game_msg in root.getElementsByTagName("GameMessageDescriptor"):
		index = game_msg.getAttribute("Index")
		opcodes = game_msg.getAttribute("NetworkIds")
		for opcode in opcodes.split(" "): 
			g_opcodes[int(opcode)] = int(index)

	for desc in root.childNodes:
		if desc.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
			index = int(desc.getAttribute("Index"))
			g_descs[index] = desc
	print "done"

	global g_log
	g_log = open("message.log", "w")

def int_to_string(reader, pad, field):
	bits = int(field.getAttribute("EncodedBits"))
	return "0x%08X (bits=%d)" % (reader.read_int(bits), bits)

def int64_to_string(reader, pad, field):
	bits = int(field.getAttribute("EncodedBits"))
	return "0x%08X (bits=%d)" % (reader.read_int64(bits), bits)

def char_array_to_string(reader, pad, field):
	length = int(field.getAttribute("ArrayLength"))
	return "%s (length=%d)" % (reader.read_char_array(length), length)

def float_to_string(reader, pad, field):
	return str(reader.read_float32())

def fixed_array_to_string(reader, pad, field):
	length = int(field.getAttribute("ArrayLength"))
	sub_name, sub_index = field.getAttribute("SubType").split("#")
	sub_desc = g_descs[int(sub_index)]

	texts = []
	for i in xrange(length):
		texts.extend(field_to_string(reader, pad, field, sub_desc))
	return texts

def optional_to_string(reader, pad, field):
	optional = reader.read_int(1)
	if optional == 1:
		sub_name, sub_index = field.getAttribute("SubType").split("#")
		sub_desc = g_descs[int(sub_index)]
		return field_to_string(reader, pad, field, sub_desc)
	return ""

g_basics = {
	"DT_INT" : int_to_string,
	"DT_INT64" : int64_to_string,
	"DT_CHARARRAY" : char_array_to_string,
	"DT_SNO" : int_to_string,
	"DT_FLOAT" : float_to_string,
	"DT_FIXEDARRAY" : fixed_array_to_string,
	"DT_GBID" : int_to_string,
	"DT_BYTE" : int_to_string,
	"DT_ENUM" : int_to_string,
	"DT_DATAID" : int_to_string,
	'DT_OPTIONAL' : optional_to_string,
	"DT_ANGLE" : float_to_string,
}

def field_to_string(reader, pad, field, desc):
	name = desc.getAttribute("Name")
	if desc.localName == "StructureDescriptor":
		texts = [ pad + name, ]
		for sub_field in desc.getElementsByTagName("Field"):
			if sub_field.hasAttribute("Type"):
				sub_type, sub_index = sub_field.getAttribute("Type").split("#")
				sub_desc = g_descs[int(sub_index)]
				texts.extend(field_to_string(reader, pad + "  ", sub_field, sub_desc))

	elif desc.localName == "BasicDescriptor":
		text_or_list = g_basics[name](reader, pad + "  ", field)
		if isinstance(text_or_list, list):
			texts = [ pad + name, ]
			texts.extend(text_or_list)
		else:
			texts = [ pad + name + " : " + text_or_list, ]

	return texts

def parse_game_msg(reader):
	if reader.get_bit_len() < 9:
		return False
		
	opcode = reader.read_int(9)
	index = g_opcodes.get(opcode)
	if index is None:
		return False

	desc = g_descs[index]
	print desc.getAttribute("Name")
	g_log.write(desc.getAttribute("Name") + "\n")

	texts = []
	for field in desc.getElementsByTagName("Field"):
		if field.hasAttribute("Type"):
			field_type, field_index = field.getAttribute("Type").split("#")
			if field_type != "RequiredMessageHeader":
				field_desc = g_descs[int(field_index)]
				texts.extend(field_to_string(reader, "  ", field, field_desc))

	for text in texts:
		print text
		g_log.write(text + "\n")

	return True

if __name__ == '__main__':
	load_xml("C:\\download\\typedescriptors.xml")