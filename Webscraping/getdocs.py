def getTargets(sources):
    """
    A function that gets all links for supported files in a KSU faculty page and downloads them locally
    supported file = 'pdf', 'doc', 'docx', 'csv', 'xls', 'html', 'txt', 'rtf', 'jpg', 'png', 'ppt', 'pptx'
    
    Input:
        Sources: A string of links and directories to tell getTargets what to get and in what directory they go 
                                                                              in the format LINK$$$$DIRCTORY\n..        
    The function also prints each successful download and its directory, and also prints each unsuccessful
    """
    supportedFiles = ('pdf', 'doc', 'docx', 'csv', 'xls', 'html', 'txt', 'rtf', 'jpg', 'png', 'ppt', 'pptx')
    for source in sources.split('\n'):
        url = source[0: source.find('$$$$')]
        directory = source[source.find('$$$$') + 4:]
        os.makedirs(os.path.dirname(directory), exist_ok=True)  # Make all needed dirs

        request = get(url)  # Get the page
        soup = BeautifulSoup(request.text, 'html.parser')  # Pass it to Beautiful Soup and extract all links (targets)
        targets = [element.find('a')['href'] for element in soup.find_all('tr') if element.find('a', href=True)]

        for target in targets:  # For every target, get target and make a file of it
            dot = target.rfind('.')  # Gets the file extension dot index
            start = target.rfind('/')  # Gets the index of start of file name

            if target[dot + 1] not in supportedFiles:  # Check for support
                print('File {} not supported'.format(target[start + 1:]))
                continue

            fetchedFile = get(target, stream=True)
            name = '{}.{}'.format(directory + target[start + 1: dot], target[dot + 1:])  # DIRECTORY.EXTENSION
            with open(name, 'wb') as file:  # Creates the file
                file.write(fetchedFile.content)
                print('Added {}'.format(name))


if __name__ == '__main__':
    from requests import get
    import os
    from bs4 import BeautifulSoup

    with open('sources.txt', 'r') as Strings:
        lines = Strings.read()
    getTargets(lines)
