
import aenum

class CentOS(aenum.Enum):
    name = 'CentOS'  # TODO: LABDESK-2900 has been opened to make sure a version is specified
    version_6 = name + ' 6'
    version_7 = name + ' 7'


print(CentOS.version_6)
