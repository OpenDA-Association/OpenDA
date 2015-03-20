/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2005  Nils van Velzen

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/**
\file  ctai_xml.h
\brief Utilities for XML access: reading/writing of trees from/to a XML file.
\note This include is only to be used internally

*/

#ifndef CTAI_XML_H
#define CTAI_XML_H

#include <libxml/encoding.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xinclude.h>
#include <libxml/xmlwriter.h>


/* COSTA-specific element properties */
#define CTAI_XML_DATABASE     (xmlChar*)("database")
#define CTAI_XML_SELECT       (xmlChar*)("select")
#define CTAI_XML_TIMEOFFSET   (xmlChar*)("timeoffset")
#define CTAI_XML_ID           (xmlChar*)("id")
#define CTAI_XML_START        (xmlChar*)("start")
#define CTAI_XML_STEP         (xmlChar*)("step")
#define CTAI_XML_STOP         (xmlChar*)("stop")
#define CTAI_XML_TAG          (xmlChar*)("tag")
#define CTAI_XML_NAME         (xmlChar*)("name")
#define CTAI_XML_DATATYPE     (xmlChar*)("datatype")
#define CTAI_XML_DIMENSION    (xmlChar*)("dimension")
#define CTAI_XML_VALUES       (xmlChar*)("values")
#define CTAI_XML_VECTOR       (xmlChar*)("vector")
#define CTAI_XML_VALUE        (xmlChar*)("value")
#define CTAI_XML_LIBRARY      (xmlChar*)("library")
#define CTAI_XML_FUNCTION     (xmlChar*)("function")
#define CTAI_XML_UNIT         (xmlChar*)("unit")
#define CTAI_XML_MISSINGVALUE (xmlChar*)("missingValue")
#define CTAI_XML_GRID         (xmlChar*)("grid")
#define CTAI_XML_CAPTION      (xmlChar*)("caption")
#define CTAI_XML_LENGTH       (xmlChar*)("length")
#define CTAI_XML_EXCLUDEFROMVECTOR      (xmlChar*)("excludeFromVector")
#define CTAI_XML_IMPLEMENTS   (xmlChar*)("implements")
#define CTAI_XML_NPROC          (xmlChar*)("nproc")
#define CTAI_XML_PARALLEL_TYPE  (xmlChar*)("parallel_type")
#define CTAI_XML_SPAWN_WORKERS  (xmlChar*)("spawn_workers")
#define CTAI_XML_NTIMES         (xmlChar*)("ntimes")
#define CTAI_XML_DUMPROCS       (xmlChar*)("dumproc")
#define CTAI_XML_SOBSCLASSNAME  (xmlChar*)("class")
#define CTAI_XML_FLAG_BARRIER   (xmlChar*)("flag_barrier")
#define CTAI_XML_T_STEP         (xmlChar*)("T_step")

#define MY_ENCODING  (xmlChar*)("ISO-8859-1")



#endif /* CTA_XML_H */
