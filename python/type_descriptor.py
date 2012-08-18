import xml.dom.minidom

import log

g_opcodes = {}
g_descs = {}

def load_xml(xml_file_path):
	dom = xml.dom.minidom.parse(xml_file_path)
	root = dom.getElementsByTagName("TypeDescriptors")[0]

	log.info("loading xml...")

	kinds = set()
	for child in root.childNodes:
		if child.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
			kinds.add(child.localName)

	for kind in kinds:
		count = len(root.getElementsByTagName(kind))
		log.info("  %10s : %d" % (kind, count))

	for game_msg in root.getElementsByTagName("GameMessageDescriptor"):
		index = game_msg.getAttribute("Index")
		opcodes = game_msg.getAttribute("NetworkIds")
		for opcode in opcodes.split(" "): 
			g_opcodes[int(opcode)] = int(index)

	for desc in root.childNodes:
		if desc.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
			index = int(desc.getAttribute("Index"))
			g_descs[index] = desc
	log.info("...loading finish")

def parse_int(reader, field):
	bits = int(field.getAttribute("EncodedBits"))
	return reader.read_int(bits)

def parse_int64(reader, field):
	bits = int(field.getAttribute("EncodedBits"))
	return reader.read_int64(bits)

def parse_char_array(reader, field):
	length = int(field.getAttribute("ArrayLength"))
	bytes = reader.read_char_array(length)
	return "(%d bytes)" % length

def parse_float(reader, field):
	return reader.read_float32()

def parse_fixed_array(reader, field):
	if field.hasAttribute("EncodedBits2"):
		length = reader.read_int(int(field.getAttribute("EncodedBits2")))
	else:
		length = int(field.getAttribute("ArrayLength"))
	sub_name, sub_index = field.getAttribute("SubType").split("#")
	sub_desc = g_descs[int(sub_index)]

	children = []
	for i in xrange(length):
		children.append(parse_field(reader, field, sub_desc))
	return children

def parse_optional(reader, field):
	optional = reader.read_int(1)
	if optional == 1:
		sub_name, sub_index = field.getAttribute("SubType").split("#")
		sub_desc = g_descs[int(sub_index)]
		return parse_field(reader, field, sub_desc)
	return None

def parse_sno_group(reader, field):
	sno_group = reader.read_int(32)
	sno_id = reader.read_int(32)
	return { "sno_group" : sno_group, "sno_id" : sno_id, }

g_basic_parser = {
	"DT_INT" : parse_int,
	"DT_INT64" : parse_int64,
	"DT_CHARARRAY" : parse_char_array,
	"DT_SNO" : parse_int,
	"DT_FLOAT" : parse_float,
	"DT_FIXEDARRAY" : parse_fixed_array,
	"DT_GBID" : parse_int,
	"DT_BYTE" : parse_int,
	"DT_ENUM" : parse_int,
	"DT_DATAID" : parse_int,
	'DT_OPTIONAL' : parse_optional,
	"DT_ANGLE" : parse_float,
	"DT_SNO_GROUP" : parse_sno_group,
}

def parse_field(reader, field, desc):
	name = desc.getAttribute("Name")
	if desc.localName == "StructureDescriptor":
		children = []
		for sub_field in desc.getElementsByTagName("Field"):
			if sub_field.hasAttribute("Type"):
				sub_type, sub_index = sub_field.getAttribute("Type").split("#")
				sub_desc = g_descs[int(sub_index)]
				children.append(parse_field(reader, sub_field, sub_desc))
		return { name : children, }

	elif desc.localName == "BasicDescriptor":
		if name not in g_basic_parser:
			raise NotImplementedError
		return { name : g_basic_parser[name](reader, field), }

def parse_game_msg(reader):
	if reader.get_bit_len() < 9:
		return False
		
	opcode = reader.read_int(9)
	index = g_opcodes.get(opcode)
	if index is None:
		return False

	desc = g_descs[index]
	log.info(desc.getAttribute("Name"))

	texts = []
	children = []
	for field in desc.getElementsByTagName("Field"):
		if field.hasAttribute("Type"):
			field_type, field_index = field.getAttribute("Type").split("#")
			if field_type != "RequiredMessageHeader":
				field_desc = g_descs[int(field_index)]
				children.append(parse_field(reader, field, field_desc))

	log.info(str(children))

	return True

if __name__ == '__main__':
	load_xml("C:\\download\\typedescriptors.xml")