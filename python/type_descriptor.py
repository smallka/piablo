import xml.dom.minidom

def main(xml_file_path):
	dom = xml.dom.minidom.parse(xml_file_path)
	root = dom.getElementsByTagName("TypeDescriptors")[0]

	kinds = set()
	for child in root.childNodes:
		if child.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
			kinds.add(child.localName)

	descs = {}
	for kind in kinds:
		descs[kind] = root.getElementsByTagName(kind)
		print kind, len(descs[kind])


if __name__ == '__main__':
	main("C:\\download\\typedescriptors.xml")