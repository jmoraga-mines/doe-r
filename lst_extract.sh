 #!/usr/bin/env bash
# Define defaults
FILETYPE="ST"
TARFILE="*_ST.tif"
NEWDIR="ST"

while [[ $# -gt 0 ]]
do
        key="$1"

        case $key in
                -s | --st)
                        FILETYPE="ST"
                        TARFILE="*_ST.tif"
                        NEWDIR="ST"
                        shift
                        ;;
                -q | --qa)
                        FILETYPE="QA"
                        TARFILE="*_PIXEL*.tif"
                        NEWDIR="QA"
                        shift
                        ;;
                -d | --directory)
                        NEWDIR="$2"
                        shift
                        shift
                        ;;
                -h | --help)
                        me=`basename "$0"`
                        # echo $'\n'
                        echo "Usage: ${me} [-s] [-q] [-d DIR] [-h]"
                        echo "${me} extracts files from  all LANDSAT-8 ADS LST Product"
                        echo "  that exist in the current directory"
                        echo "  -s Extracts Land Surface Temperature files"
                        echo "  -q Extracts LST PIXEL Quality files"
                        echo "  -d Creates and extracts files to directory DIR"
                        echo "  -h Displays this help"
                        exit 0
                        ;;
                *)
                        echo "ERROR: Unknown flag " $key
                        shift
                        exit -1
                        ;;
        esac
done

FILEPATTERN="*${FILETYPE}.tar"
# Create directories and subdirectories
mkdir -p $NEWDIR

# example
# tar xf LC08_CU_004008_20171209_20190429_C01_V01_QA.tar --wildcards "*PIXEL*"
for f in `ls $FILEPATTERN`; do
        tar xf $f --directory $NEWDIR --wildcards $TARFILE
done
