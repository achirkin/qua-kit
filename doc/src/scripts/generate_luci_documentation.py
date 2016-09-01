# -*- coding: utf-8 -*-

import re
import sys
import json
import jinja2
from bs4 import BeautifulSoup

LATEX_SUBS = (
    (re.compile(r'\\'), r'\\textbackslash'),
    (re.compile(r'([{}_#%&$])'), r'\\\1'),
    (re.compile(r'~'), r'\~{}'),
    (re.compile(r'\^'), r'\^{}'),
    (re.compile(r'"'), r"''"),
    (re.compile(r'\.\.\.+'), r'\\ldots'),
)

def escape_tex(value):
    newval = value
    for pattern, replacement in LATEX_SUBS:
        newval = pattern.sub(replacement, newval)
    return newval

def create_json(js, comment):
    if isinstance(js, str) or isinstance(js, unicode):
        return js + ("// {}".format(comment) if comment != "" else "")
    else:
        if isinstance(js, list) and not isinstance(js[0], dict):
            return js
        print js
        ret = {}
        for child in js:
            ret[child["name"]] = create_json(child["type"], child["comment"])
        return ret

def generate_tex(js):
    texenv = jinja2.Environment()
    texenv.block_start_string = '((*'
    texenv.block_end_string = '*))'
    texenv.variable_start_string = '((('
    texenv.variable_end_string = ')))'
    texenv.comment_start_string = '((='
    texenv.comment_end_string = '=))'
    texenv.trim_blocks = True
    texenv.filters['escape_tex'] = escape_tex
    texenv.loader = jinja2.FileSystemLoader("src/scripts/templates/")
    template = texenv.get_template('service.tex')

    for service in js:
        for i in range(len(js[service]["inputs"])):
            js[service]["inputs"][i]["json"] = json.dumps(create_json(js[service]["inputs"][i]["type"],""), indent=2, sort_keys=True)
        for i in range(len(js[service]["outputs"])):
            js[service]["outputs"][i]["json"] = json.dumps(create_json(js[service]["outputs"][i]["type"],""), indent=2, sort_keys=True)
    return template.render(services=js)

def parse_input(soup):
    name = soup.get_text().split(':')[0].strip()

    comment = soup.find(class_='comment', recursive=False)
    if comment is not None:
        comment = comment.get_text().strip().strip("\/\/").strip().replace('\n', ' ')
    else:
        comment = ""

    inputs = soup.find('ul')
    if inputs is not None:
        inputs = map(parse_input, inputs.find_all('li', recursive=False))
    else:
        inputs = []

    type = ""
    if len(inputs) == 0:
        try:
            type = re.match(r'(?:\"([^\"]+)\")|(\[[^\[\]]+\])(?:.*)', soup.get_text().split(':')[1].strip())
            type = type.group(1) if type.group(1) is not None else json.loads(type.group(2))
        except:
            type = "error"
    else:
        type = inputs

    dr = {"name" : name, "comment" : comment.replace("\n", " "), "type": type}
    return dr

def parse_service(soup):
    name = soup.find('h1').get_text()
    description = soup.find(class_='descr').get_text()
    inputs = map(parse_input, soup.find(class_='inputs', recursive=False).find_all('li', recursive=False))
    outputs = map(parse_input, soup.find(class_='outputs', recursive=False).find_all('li', recursive=False))
    return (name.strip('"'), {"description" : description, "inputs" : inputs, "outputs" : outputs})


if __name__ == '__main__':
    with open(sys.argv[1]) as fh:
        doc = fh.read()
        soup = BeautifulSoup(doc, 'html.parser')
        services = map(parse_service, soup.find(id='list').find_all('div'))

        js = {}
        for service in services:
            js[service[0]] = service[1]

        # schema output
        # print json.dumps(js)

        with open(sys.argv[2], 'w') as fh_out:
            fh_out.write(generate_tex(js))
