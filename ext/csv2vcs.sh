### snip - start of script - remove this line ###
#!/bin/bash
# use a 'here document' to create the Vcard format
## add/remove Vcard fields between the 'EOM' start/end marks
function create_vcard {
cat << EOM
BEGIN:VCARD
VERSION:3.0

TEL;TYPE=CELL:${CELL_NUMBER}
END:VCARD
EOM
}
###

IN_FILE=$1
## if IN_FILE missing show usage
if [[ "${IN_FILE}" == "" ]] ; then printf "\n\tUsage: $0 Input_file_name\n\n" ; exit 1 ; fi

OUT=${IN_FILE}.vcard
## if OUT already exists then rename
if [[ -e ${OUT} ]] ; then mv ${OUT} ${OUT}.last ; fi

for CELL_NUMBER in $(cat ${IN_FILE})
do
    create_vcard >> ${OUT}
done
ls -l ${OUT}
### snip - end of script ###
