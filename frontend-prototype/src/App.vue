<template>
  <div id="app" class="mdc-typography">
    <router-view></router-view>
  </div>
</template>

<script>
export default {
  name: 'app'
}
</script>

<style lang="scss">
@import "~material-components-web/dist/material-components-web.css";

body {
  padding: 0;
  margin: 0;
  overflow: hidden;
}
</style>
