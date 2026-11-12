{{- define "gams-miro-server.v1.customFiles" }}
  {{- if .Values.proxy.config.logo }}
logo.png: {{ .Values.proxy.config.logo }}
  {{- end }}

  {{- if .Values.proxy.config.favicon }}
favicon.ico: {{ .Values.proxy.config.favicon }}
  {{- end }}

  {{- if eq .Values.proxy.config.colorTheme "custom" }}
    {{- $content := .Values.proxy.config.customTheme }}
    {{- if eq $content "" }}
      {{- fail "Values.proxy.config.colorTheme is 'custom' but '.Values.proxy.config.customTheme' does not contain any data." }}
    {{- end }}
colors_custom.css: {{ $content }}
  {{- end }}
{{- end }}
