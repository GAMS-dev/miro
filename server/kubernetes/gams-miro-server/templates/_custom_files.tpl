{{- define "gams-miro-server.v1.customFiles" }}
  {{- if .Values.proxy.config.logo.enabled }}
    {{- $content := .Values.proxy.config.logo.base64 | default (.Files.Get (.Values.proxy.config.logo.path | default "") | b64enc) }}
    {{- if eq $content "" }}
      {{- fail "Values.proxy.config.logo.enabled is 'true' but neither Values.proxy.config.logo.base64 nor Values.proxy.config.logo.path contains any data." }}
    {{- end }}
logo.png: {{ $content }}
  {{- end }}

  {{- if .Values.proxy.config.favicon.enabled }}
    {{- $content := .Values.proxy.config.favicon.base64 | default (.Files.Get (.Values.proxy.config.favicon.path | default "") | b64enc) }}
    {{- if eq $content "" }}
      {{- fail "Values.proxy.config.favicon.enabled is 'true' but neither Values.proxy.config.favicon.base64 nor Values.proxy.config.favicon.path contains any data." }}
    {{- end }}
favicon.ico: {{ $content }}
  {{- end }}

  {{- if .Values.proxy.config.customCSS.enabled }}
    {{- $content := .Values.proxy.config.customCSS.base64 | default (.Files.Get (.Values.proxy.config.customCSS.path | default "") | b64enc) }}
    {{- if eq $content "" }}
      {{- fail "Values.proxy.config.customCSS.enabled is 'true' but neither Values.proxy.config.customCSS.base64 nor Values.proxy.config.customCSS.path contains any data." }}
    {{- end }}
custom.css: {{ $content }}
  {{- end }}

  {{- if eq .Values.proxy.config.colorTheme "custom" }}
    {{- $content := .Files.Get "resources/colors_custom.css" }}
    {{- if eq $content "" }}
      {{- fail "Values.proxy.config.colorTheme is 'custom' but 'resources/colors_custom.css' does not exist." }}
    {{- end }}
colors_custom.css: {{ $content | b64enc }}
  {{- end }}
{{- end }}
