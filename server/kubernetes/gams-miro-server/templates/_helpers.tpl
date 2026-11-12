{{- define "gams-miro-server.v1.renderLabelsAsPatches" }}
{{- $labelsList := . }}
{{- range $i, $label := $labelsList -}}
{{- if $i -}}{{- "\n" -}}{{- end -}}
- op: add
  path: /metadata/labels/{{ $label.key | replace "~" "~0" | replace "/" "~1" }}
  value: {{ $label.value | quote }}
{{- end -}}
{{- end -}}
