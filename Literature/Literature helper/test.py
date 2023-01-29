import regex as re


R_NAME = r'([\p{L}]+(?:\s[\p{L}]\.)?)' # Matches Newman L., or Newman, with any special characters included

author = f'Mason, Geoff, Max Nathan, and Anna Rosso'
authors = []


for i in range(1,4):
    first_author = f'{R_NAME}, {R_NAME}, '
    additional_author = f'{R_NAME} {R_NAME}, '
    last_author = f'and {R_NAME} {R_NAME}'
    more_authors_rgx = first_author + i*additional_author + last_author
    print(more_authors_rgx)
    more_authors_match = re.search(more_authors_rgx, author)
    if more_authors_match:
        author_name = more_authors_match.group(1)
        new_name = f'{author_name} et al.'
        authors.append(new_name)


print(authors)