import xml.dom.minidom

g_opcodes = {}
g_descs = {}

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

def parse_int(reader, pad, desc):
	bits = int(desc.getAttribute("EncodedBits"))
	print pad + "DT_INT : " + str(reader.read_int(bits))

def parse_int64(reader, pad, desc):
	bits = int(desc.getAttribute("EncodedBits"))
	print pad + "DT_INT64 : " + str(reader.read_int64(bits))

g_basics = {
	"DT_INT" : parse_int,
	"DT_INT64" : parse_int64,
}

def parse_fields(reader, pad, fields):
	for field in fields:
		if field.hasAttribute("Type"):
			name, index = field.getAttribute("Type").split("#")
			index = int(index)
			desc = g_descs[index]

			if desc.localName == "StructureDescriptor":
				print pad + name
				parse_fields(reader, pad + "  ", desc.getElementsByTagName("Field"))
			elif desc.localName == "BasicDescriptor":
				g_basics[name](reader, pad, field)

def parse_game_msg(reader):
	if reader.get_bit_len() < 9:
		return False
		
	opcode = reader.read_int(9)
	index = g_opcodes.get(opcode)
	if index is None:
		return False

	desc = g_descs[index]
	print desc.getAttribute("Name")

	fields = []
	for field in desc.getElementsByTagName("Field"):
		if field.hasAttribute("Type"):
			field_name, field_index = field.getAttribute("Type").split("#")
			if field_name != "RequiredMessageHeader":
				fields.append(field)

	parse_fields(reader, "  ", fields)

	return True

if __name__ == '__main__':
	load_xml("C:\\download\\typedescriptors.xml")