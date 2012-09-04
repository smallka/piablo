import xml.dom.minidom

import log
from alias import MessageAlias, FieldAlias

g_attr = {}
g_opcodes = {}
g_descs = {}

IDX_INDEX = "index"
IDX_NAME = "name"

def load_xml(attributes_xml, type_descriptors_xml):
    dom = xml.dom.minidom.parse(attributes_xml)
    root = dom.getElementsByTagName("Attributes")[0]

    log.info("loading attributes...")

    for entry in root.getElementsByTagName("Entry"):
        index = int(entry.getAttribute("Id"))
        g_attr[index] = entry

    dom = xml.dom.minidom.parse(type_descriptors_xml)
    root = dom.getElementsByTagName("TypeDescriptors")[0]

    log.info("loading type descriptors...")

    kinds = set()
    for child in root.childNodes:
        if child.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
            kinds.add(child.localName)

    for kind in kinds:
        count = len(root.getElementsByTagName(kind))
        log.info("  %10s : %d" % (kind, count))

    for game_msg in root.getElementsByTagName("GameMessageDescriptor"):
        index = game_msg.getAttribute("Index")
        name = game_msg.getAttribute("Name")
        opcodes = game_msg.getAttribute("NetworkIds")
        opcodes = opcodes.split(" ")
        if len(opcodes) > 1:
            name_index = 1 
            for opcode in opcodes: 
                alias = "%s%d" % (name, name_index)
                if alias in MessageAlias:
                    alias = MessageAlias[alias]
                g_opcodes[int(opcode)] = {
                    IDX_INDEX : int(index),
                    IDX_NAME : alias,
                }
                name_index += 1
        else:
            g_opcodes[int(opcodes[0])] = {
                IDX_INDEX : int(index),
                IDX_NAME : name,
            }

    for desc in root.childNodes:
        if desc.nodeType == xml.dom.minidom.Node.ELEMENT_NODE:
            index = int(desc.getAttribute("Index"))
            g_descs[index] = desc

def parse_attribute(reader, fields):
    index = int(fields[1]["DT_INT"])
    attr = g_attr[index]
    attr_type = attr.getAttribute("EncodingType")
    if attr_type == "Int":
        bit_count = int(attr.getAttribute("BitCount"))
        value = reader.read_int(bit_count)
    elif attr_type == "IntMinMax":
        # TODO: IntMinMax
        bit_count = int(attr.getAttribute("BitCount"))
        value = reader.read_int(bit_count)
    elif attr_type == "Float16":
        # TODO: Float16
        value = reader.read_int(16)
    elif attr_type == "Float16Or32":
        if reader.read_int(1) == 1:
            # TODO: Float16
            value = reader.read_int(16)
        else:
            value = reader.read_float32()
    elif attr_type == "Float32":
        value = reader.read_float32()
    else:
        log.error("unknown attribute type: " + attr_type)
        raise NotImplementedError
    key = attr.getAttribute("Name") + "::" + attr_type
    fields.append({ key : value, })

def parse_int(reader, field):
    bits = int(field.getAttribute("EncodedBits"))
    return reader.read_int(bits)

def parse_int64(reader, field):
    bits = int(field.getAttribute("EncodedBits"))
    return reader.read_int64(bits)

def parse_char_array(reader, field):
    length = int(field.getAttribute("ArrayLength"))
    bytes = reader.read_char_array(length)
    # TODO: represent text
    return "(%d bytes)" % length

def parse_float32(reader, field):
    return reader.read_float32()

def parse_fixed_array(reader, field):
    if field.hasAttribute("EncodedBits2"):
        length = reader.read_int(int(field.getAttribute("EncodedBits2")))
    else:
        length = int(field.getAttribute("ArrayLength"))
    sub_type, sub_index = field.getAttribute("SubType").split("#")
    sub_desc = g_descs[int(sub_index)]

    children = []
    for i in xrange(length):
        children.append(parse_field(reader, field, sub_desc))
    if sub_type == "NetAttributeKeyValue":
        for child in children:
            parse_attribute(reader, child)
    return children

def parse_optional(reader, field):
    optional = reader.read_int(1)
    if optional == 1:
        sub_type, sub_index = field.getAttribute("SubType").split("#")
        sub_desc = g_descs[int(sub_index)]
        return parse_field(reader, field, sub_desc)
    return None

def parse_sno_group(reader, field):
    return { }

def parse_sno_handle(reader, field):
    sno_group = reader.read_int(32)
    sno_id = reader.read_int(32)
    return { "sno_group" : sno_group, "sno_id" : sno_id, }

def parse_variable_array(reader, field):
    raise NotImplementedError

g_basic_parser = {
    "DT_INT" : parse_int,
    "DT_INT64" : parse_int64,
    "DT_CHARARRAY" : parse_char_array,
    "DT_SNO" : parse_int,
    "DT_FLOAT" : parse_float32,
    "DT_FIXEDARRAY" : parse_fixed_array,
    "DT_GBID" : parse_int,
    "DT_BYTE" : parse_int,
    "DT_ENUM" : parse_int,
    "DT_DATAID" : parse_int,
    'DT_OPTIONAL' : parse_optional,
    "DT_ANGLE" : parse_float32,
    "DT_SNO_GROUP" : parse_sno_group,
    "DT_SNONAME_HANDLE" : parse_sno_handle,
    "DT_VARIABLEARRAY" : parse_variable_array,
}

def get_field_alias(name, field_type, field):
    if not field.hasAttribute("Offset"):
        log.error("offset missing, attr: %s, type: %s" % (name, field_type))
        raise NotImplementedError
    offset = int(field.getAttribute("Offset"))
    if name in FieldAlias and offset in FieldAlias[name]:
        return "%s::%s" % (FieldAlias[name][offset], field_type)
    return field_type

def parse_field(reader, field, desc):
    name = desc.getAttribute("Name")
    if desc.localName == "StructureDescriptor":
        children = []
        for sub_field in desc.getElementsByTagName("Field"):
            if sub_field.hasAttribute("Type"):
                sub_type, sub_index = sub_field.getAttribute("Type").split("#")
                key = get_field_alias(name, sub_type, sub_field)
                sub_desc = g_descs[int(sub_index)]
                children.append({ key : parse_field(reader, sub_field, sub_desc), })
                if sub_type == "NetAttributeKeyValue":
                    log.error("unexpected NetAttributeKeyValue in %s" % name)
                    raise NotImplementedError
        return children

    elif desc.localName == "BasicDescriptor":
        if name not in g_basic_parser:
            log.error("unsupported basic descriptor: " + name)
            raise NotImplementedError
        return g_basic_parser[name](reader, field)

def parse_game_msg(reader):
    if reader.get_bit_len() < 9:
        return None
        
    opcode = reader.read_int(9)
    op_info = g_opcodes.get(opcode)
    if op_info is None:
        return None

    desc = g_descs[op_info[IDX_INDEX]]
    name = op_info[IDX_NAME]
    #log.info(name)

    children = []
    try:
        for field in desc.getElementsByTagName("Field"):
            if field.hasAttribute("Type"):
                field_type, field_index = field.getAttribute("Type").split("#")
                if field_type != "RequiredMessageHeader":
                    key = get_field_alias(name, field_type, field)
                    field_desc = g_descs[int(field_index)]
                    child = parse_field(reader, field, field_desc)
                    if field_type == "NetAttributeKeyValue":
                        parse_attribute(reader, child)
                    children.append({ key : child, })
    except NotImplementedError as e:
        log.error("error when parsing, name: %s, field_type: %s" % (name, field_type))
        raise e

    #log.info(str(children))
    return name, children

def text_in_msg(msg, text):
    if isinstance(msg, list):
        for item in msg:
            if text_in_msg(item, text):
                return True
    elif isinstance(msg, dict):
        for k, v in msg.iteritems():
            if text_in_msg(v, text):
                return True
    elif str(msg) == text:
        return True
    return False

if __name__ == '__main__':
    load_xml("attributes.xml", "typedescriptors.xml")